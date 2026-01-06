import { defineStore } from 'pinia';
import { ref, computed } from 'vue';
import { DextHubConnection } from '@/lib/dext-hubs.js';

export interface LogEntry {
    id: number;
    timestamp: Date;
    level: 'info' | 'warn' | 'error' | 'success' | 'debug';
    message: string;
}

export const useHubStore = defineStore('hub', () => {
    // State
    const connection = ref<DextHubConnection | null>(null);
    const status = ref<'disconnected' | 'connecting' | 'connected' | 'reconnecting'>('disconnected');
    const logs = ref<LogEntry[]>([]);
    const lastError = ref<string | null>(null);

    // Getters
    const isConnected = computed(() => status.value === 'connected');

    // Actions
    function initialize() {
        if (connection.value) return;

        // Connect to /hubs/dashboard (will create later in backend)
        // For dev proxy, this goes to localhost:3000/hubs/dashboard
        const conn = new DextHubConnection('/hubs/dashboard', {
            reconnect: true
        });

        conn.on('ConnectionStateChanged', (newState: any) => {
            status.value = newState;
        });

        conn.on('LogMessage', (level: string, message: string) => {
            addLog(level as any, message);
        });

        // Handle generic errors
        conn.on('Error', (err: any) => {
            console.error('Hub Error:', err);
            lastError.value = err.toString();
            addLog('error', `Hub Error: ${err}`);
        });

        connection.value = conn;

        // Start connection
        conn.start().catch(err => {
            console.warn('Hub connection failed (expected if backend not ready):', err);
            status.value = 'disconnected';
            // Retry logic is inside DextHubConnection, but initial start fail needs handling
        });
    }

    function addLog(level: 'info' | 'warn' | 'error' | 'success' | 'debug', message: string) {
        logs.value.push({
            id: Date.now() + Math.random(),
            timestamp: new Date(),
            level,
            message
        });

        // Keep only last 1000 logs
        if (logs.value.length > 1000) {
            logs.value.shift();
        }
    }

    function clearLogs() {
        logs.value = [];
    }

    return {
        status,
        isConnected,
        logs,
        initialize,
        addLog,
        clearLogs
    };
});
