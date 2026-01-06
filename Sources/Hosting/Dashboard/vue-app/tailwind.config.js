/** @type {import('tailwindcss').Config} */
export default {
    content: [
        "./index.html",
        "./src/**/*.{vue,js,ts,jsx,tsx}",
    ],
    darkMode: 'class',
    theme: {
        extend: {
            colors: {
                background: '#0f172a', // Slate-900
                surface: '#1e293b',    // Slate-800
                text: '#e2e8f0',       // Slate-200
                primary: '#00ff9d',    // Neon Green
                secondary: '#06b6d4',  // Cyan
                accent: '#10b981',     // Emerald
                danger: '#ef4444',     // Red-500
                warning: '#f59e0b',    // Amber-500
            },
            fontFamily: {
                sans: ['Inter', 'sans-serif'],
                mono: ['JetBrains Mono', 'Fira Code', 'consolas', 'monospace'],
            }
        },
    },
    plugins: [],
}
