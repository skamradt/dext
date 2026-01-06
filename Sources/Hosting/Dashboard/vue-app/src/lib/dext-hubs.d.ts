export class DextHubConnection {
    constructor(url: string, options?: any);
    on(methodName: string, handler: Function): this;
    off(methodName: string, handler?: Function): this;
    start(): Promise<void>;
    stop(): Promise<void>;
    invoke(methodName: string, ...args: any[]): Promise<any>;
    send(methodName: string, ...args: any[]): Promise<void>;
    readonly connectionState: string;
}
