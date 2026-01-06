/**
 * Dext Hub Connection Client
 * 
 * SignalR-compatible JavaScript client for Dext.Hubs
 * 
 * Usage:
 *   const connection = new DextHubConnection('/hubs/dashboard');
 *   connection.on('ReceiveMessage', (msg) => console.log(msg));
 *   await connection.start();
 *   await connection.invoke('SendMessage', 'Hello!');
 * 
 * @author Cesar Romero
 * @license Apache-2.0
 */

class DextHubConnection {
  /**
   * Creates a new Hub connection
   * @param {string} hubUrl - The Hub endpoint URL (e.g., '/hubs/dashboard')
   * @param {Object} options - Connection options
   */
  constructor(hubUrl, options = {}) {
    this.hubUrl = hubUrl;
    this.options = {
      transport: 'serverSentEvents', // or 'longPolling'
      reconnect: true,
      reconnectDelay: 3000,
      ...options
    };

    this.connectionId = null;
    this.eventSource = null;
    this.handlers = new Map();
    this.state = 'disconnected'; // disconnected, connecting, connected, reconnecting
    this.invocationId = 0;
    this.pendingInvocations = new Map();
  }

  /**
   * Registers a handler for a Hub method
   * @param {string} methodName - The method name to handle
   * @param {Function} handler - The handler function
   */
  on(methodName, handler) {
    if (!this.handlers.has(methodName)) {
      this.handlers.set(methodName, []);
    }
    this.handlers.get(methodName).push(handler);
    return this;
  }

  /**
   * Removes a handler for a Hub method
   * @param {string} methodName - The method name
   * @param {Function} handler - The handler to remove (optional, removes all if not specified)
   */
  off(methodName, handler) {
    if (!handler) {
      this.handlers.delete(methodName);
    } else if (this.handlers.has(methodName)) {
      const handlers = this.handlers.get(methodName);
      const index = handlers.indexOf(handler);
      if (index > -1) {
        handlers.splice(index, 1);
      }
    }
    return this;
  }

  /**
   * Starts the Hub connection
   * @returns {Promise<void>}
   */
  async start() {
    if (this.state === 'connected') {
      return;
    }

    this.state = 'connecting';

    try {
      // Step 1: Negotiate
      const negotiateResponse = await this._negotiate();
      this.connectionId = negotiateResponse.connectionId;

      // Step 2: Connect transport
      if (this.options.transport === 'serverSentEvents') {
        await this._connectSSE();
      } else {
        await this._connectLongPolling();
      }

      this.state = 'connected';
      this._triggerHandlers('connected', { connectionId: this.connectionId });

    } catch (error) {
      this.state = 'disconnected';
      throw error;
    }
  }

  /**
   * Stops the Hub connection
   * @returns {Promise<void>}
   */
  async stop() {
    this._stopPolling();

    if (this.eventSource) {
      this.eventSource.close();
      this.eventSource = null;
    }

    this.state = 'disconnected';
    this.connectionId = null;
    this._triggerHandlers('disconnected', {});
  }

  /**
   * Invokes a Hub method on the server
   * @param {string} methodName - The method to invoke
   * @param {...any} args - Arguments to pass
   * @returns {Promise<any>} The method result
   */
  async invoke(methodName, ...args) {
    if (this.state !== 'connected') {
      throw new Error('Cannot invoke: not connected');
    }

    const invocationId = String(++this.invocationId);

    const request = {
      type: 1, // Invocation
      invocationId: invocationId,
      target: methodName,
      arguments: args
    };

    return new Promise(async (resolve, reject) => {
      this.pendingInvocations.set(invocationId, { resolve, reject });

      try {
        const response = await fetch(`${this.hubUrl}?id=${this.connectionId}`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(request)
        });

        const result = await response.json();

        this.pendingInvocations.delete(invocationId);

        if (result.error) {
          reject(new Error(result.error));
        } else {
          resolve(result.result);
        }
      } catch (error) {
        this.pendingInvocations.delete(invocationId);
        reject(error);
      }
    });
  }

  /**
   * Sends a message without waiting for result
   * @param {string} methodName - The method to invoke
   * @param {...any} args - Arguments to pass
   */
  async send(methodName, ...args) {
    if (this.state !== 'connected') {
      throw new Error('Cannot send: not connected');
    }

    const request = {
      type: 1, // Invocation
      target: methodName,
      arguments: args
    };

    await fetch(`${this.hubUrl}?id=${this.connectionId}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(request)
    });
  }

  /**
   * Negotiate connection
   * @private
   */
  async _negotiate() {
    const response = await fetch(`${this.hubUrl}/negotiate`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      }
    });

    if (!response.ok) {
      throw new Error(`Negotiate failed: ${response.status}`);
    }

    return await response.json();
  }

  /**
   * Connect using polling (SSE doesn't flush properly with Indy)
   * @private
   */
  async _connectSSE() {
    // Use polling instead of SSE - more reliable with Indy HTTP server
    this._startPolling();
    return Promise.resolve();
  }

  /**
   * Start polling for messages
   * @private
   */
  _startPolling() {
    if (this._pollingInterval) {
      clearInterval(this._pollingInterval);
    }

    this._pollingInterval = setInterval(async () => {
      if (this.state !== 'connected') {
        clearInterval(this._pollingInterval);
        return;
      }

      try {
        const response = await fetch(`${this.hubUrl}/poll?id=${this.connectionId}`);
        if (response.ok) {
          const messages = await response.json();
          if (Array.isArray(messages)) {
            for (const msg of messages) {
              this._handleMessage(msg);
            }
          }
        }
      } catch (error) {
        console.error('Polling error:', error);
      }
    }, 500);
  }

  /**
   * Stop polling
   * @private
   */
  _stopPolling() {
    if (this._pollingInterval) {
      clearInterval(this._pollingInterval);
      this._pollingInterval = null;
    }
  }

  /**
   * Connect using Long Polling (fallback)
   * @private
   */
  async _connectLongPolling() {
    this._startPolling();
    return Promise.resolve();
  }

  /**
   * Handle incoming message
   * @private
   */
  _handleMessage(data) {
    try {
      // Remove record separator if present
      const cleanData = data.replace(/\x1e$/, '');
      if (!cleanData) return;

      const message = JSON.parse(cleanData);

      switch (message.type) {
        case 1: // Invocation
          this._handleInvocation(message);
          break;
        case 3: // Completion
          this._handleCompletion(message);
          break;
        case 6: // Ping
          // Keep-alive, no action needed
          break;
        case 7: // Close
          this._handleClose(message);
          break;
      }
    } catch (error) {
      console.error('Error handling message:', error, data);
    }
  }

  /**
   * Handle Hub method invocation from server
   * @private
   */
  _handleInvocation(message) {
    const { target, arguments: args } = message;
    this._triggerHandlers(target, ...(args || []));
  }

  /**
   * Handle completion message
   * @private
   */
  _handleCompletion(message) {
    const pending = this.pendingInvocations.get(message.invocationId);
    if (pending) {
      this.pendingInvocations.delete(message.invocationId);
      if (message.error) {
        pending.reject(new Error(message.error));
      } else {
        pending.resolve(message.result);
      }
    }
  }

  /**
   * Handle close message
   * @private
   */
  _handleClose(message) {
    console.log('Hub connection closed:', message.error || 'No error');
    this.stop();
  }

  /**
   * Handle reconnection
   * @private
   */
  async _handleReconnect() {
    if (this.state === 'reconnecting') return;

    this.state = 'reconnecting';
    console.log('Attempting to reconnect...');

    if (this.eventSource) {
      this.eventSource.close();
      this.eventSource = null;
    }

    setTimeout(async () => {
      try {
        await this.start();
        console.log('Reconnected successfully');
      } catch (error) {
        console.error('Reconnection failed:', error);
        this._handleReconnect(); // Try again
      }
    }, this.options.reconnectDelay);
  }

  /**
   * Trigger handlers for a method
   * @private
   */
  _triggerHandlers(methodName, ...args) {
    const handlers = this.handlers.get(methodName);
    if (handlers) {
      handlers.forEach(handler => {
        try {
          handler(...args);
        } catch (error) {
          console.error(`Error in handler for ${methodName}:`, error);
        }
      });
    }
  }

  /**
   * Gets the current connection state
   */
  get connectionState() {
    return this.state;
  }
}

// Export for different module systems
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { DextHubConnection };
} else if (typeof window !== 'undefined') {
  window.DextHubConnection = DextHubConnection;
}
