<template>
  <aside class="w-16 h-screen bg-surface border-r border-slate-700 flex flex-col items-center py-4 fixed left-0 top-0 z-50">
    <!-- Logo -->
    <div class="mb-8 w-10 h-10 bg-primary rounded-lg flex items-center justify-center text-slate-900 font-bold">
      D
    </div>

    <!-- Nav Items -->
    <nav class="flex-1 flex flex-col gap-4 w-full px-2">
      <router-link to="/" 
          class="w-12 h-12 flex items-center justify-center rounded-xl text-slate-400 hover:bg-slate-700 hover:text-primary transition-all duration-200 cursor-pointer" 
          active-class="bg-slate-700 text-primary shadow-lg shadow-primary/20 border border-slate-600"
          title="Dashboard">
        <span class="text-xl">🏠</span>
      </router-link>
      
      <router-link to="/tests" 
          class="w-12 h-12 flex items-center justify-center rounded-xl text-slate-400 hover:bg-slate-700 hover:text-primary transition-all duration-200 cursor-pointer" 
          active-class="bg-slate-700 text-primary shadow-lg shadow-primary/20 border border-slate-600"
          title="Tests">
        <span class="text-xl">🧪</span>
      </router-link>
      
      <router-link to="/run" 
          class="w-12 h-12 flex items-center justify-center rounded-xl text-slate-400 hover:bg-slate-700 hover:text-primary transition-all duration-200 cursor-pointer" 
          active-class="bg-slate-700 text-primary shadow-lg shadow-primary/20 border border-slate-600"
          title="Run & Build">
        <span class="text-xl">▶️</span>
      </router-link>

      <div class="flex-1"></div>

      <router-link to="/settings" 
          class="w-12 h-12 flex items-center justify-center rounded-xl text-slate-400 hover:bg-slate-700 hover:text-primary transition-all duration-200 cursor-pointer mb-4" 
          active-class="bg-slate-700 text-primary shadow-lg shadow-primary/20 border border-slate-600"
          title="Settings">
        <span class="text-xl">⚙️</span>
      </router-link>

      <!-- Connection Status -->
      <div class="mb-4 w-3 h-3 rounded-full transition-colors duration-500" 
           :class="{
              'bg-emerald-500 shadow-[0_0_10px_rgba(16,185,129,0.5)]': isConnected,
              'bg-red-500 animate-pulse': !isConnected && status !== 'connecting',
              'bg-yellow-500 animate-bounce': status === 'connecting'
           }"
           :title="'Status: ' + status">
      </div>
    </nav>
  </aside>
</template>

<script setup lang="ts">
import { storeToRefs } from 'pinia';
import { useHubStore } from '@/stores/hubStore';

const store = useHubStore();
const { status, isConnected } = storeToRefs(store);
</script>
