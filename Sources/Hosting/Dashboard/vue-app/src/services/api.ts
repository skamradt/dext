export interface Project {
    name: string;
    path: string;
    lastAccess: string;
}

export interface Config {
    dextPath: string;
    coveragePath: string;
    configPath: string;
    environments: Environment[];
}

export interface Environment {
    version: string;
    name: string;
    path: string;
    isDefault: boolean;
    platforms: string[];
}

export interface TestSummary {
    available: boolean;
    coverage?: number;
    path: string;
}

const API_BASE = '/api';

// In development (vite server), we need to proxy to real CLI backend or mock
// For now, let's assume we can configure proxy in vite.config.ts

export async function fetchProjects(): Promise<Project[]> {
    const res = await fetch(`${API_BASE}/projects`);
    if (!res.ok) throw new Error('Failed to fetch projects');
    return res.json();
}

export async function fetchConfig(): Promise<Config> {
    const res = await fetch(`${API_BASE}/config`);
    return res.json();
}

export async function fetchTestSummary(): Promise<TestSummary> {
    const res = await fetch(`${API_BASE}/test/summary`);
    return res.json();
}
