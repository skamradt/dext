<template>
  <div class="p-8">
    <div class="flex justify-between items-end mb-8">
      <div>
        <h1 class="text-3xl font-bold text-white mb-2">Projects</h1>
        <p class="text-slate-400">Manage your Dext applications.</p>
      </div>
      <button @click="loadData" class="px-4 py-2 bg-surface hover:bg-slate-700 rounded-lg text-sm text-white transition-colors border border-white/10 flex items-center gap-2">
        <span>↻</span> Refresh
      </button>
    </div>

    <div v-if="loading" class="text-center py-12">
      <div class="animate-spin text-4xl mb-4">🌀</div>
      <p class="text-slate-500">Loading projects...</p>
    </div>

    <div v-else-if="error" class="bg-red-500/10 border border-red-500/20 text-red-200 p-6 rounded-xl">
      {{ error }}
      <p class="text-sm mt-2 text-red-400">Make sure "dext ui" is running on port 3000.</p>
    </div>

    <div v-else-if="projects.length === 0" class="col-span-full py-20 text-center border-2 border-dashed border-slate-700 rounded-2xl bg-surface/20">
      <p class="text-xl text-slate-300 font-semibold mb-2">No projects found</p>
      <p class="text-slate-500">Run "dext" in a project folder to register it.</p>
    </div>

    <div v-else class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
      <div v-for="p in projects" :key="p.path" class="bg-surface rounded-xl p-6 relative group overflow-hidden transition-all hover:scale-[1.01] hover:shadow-xl hover:shadow-cyan-900/10 border border-slate-700/50 hover:border-cyan-500/30">
        <div class="absolute top-0 right-0 p-4 opacity-0 group-hover:opacity-100 transition-opacity">
           <svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5 text-cyan-400" viewBox="0 0 20 20" fill="currentColor"><path d="M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z" /></svg>
        </div>
        <h3 class="text-lg font-semibold text-white mb-1 group-hover:text-cyan-400 transition-colors">{{ p.name }}</h3>
        <p class="text-xs text-slate-400 break-all mb-4 h-8 overflow-hidden text-ellipsis font-mono opacity-80">{{ p.path }}</p>
        <div class="flex items-center gap-2">
            <span class="w-2 h-2 rounded-full bg-emerald-500"></span>
            <span class="text-xs text-slate-500">Last access: {{ new Date(p.lastAccess).toLocaleDateString() }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue';
import { fetchProjects, type Project } from '@/services/api';

const projects = ref<Project[]>([]);
const loading = ref(true);
const error = ref('');

async function loadData() {
  loading.value = true;
  error.value = '';
  try {
    projects.value = await fetchProjects();
  } catch (e: any) {
    error.value = e.message || 'Failed to connect to Dext CLI';
    // For demo purposes if API fails (when no local server), mock data
    if (import.meta.env.DEV) {
       console.warn('Using mock data');
       projects.value = [
         { name: 'Dext.Core', path: 'C:\\dev\\Dext\\Core', lastAccess: new Date().toISOString() },
         { name: 'MyApi', path: 'C:\\projects\\MyApi', lastAccess: new Date().toISOString() }
       ];
       error.value = '';
    }
  } finally {
    loading.value = false;
  }
}

onMounted(() => {
  loadData();
});
</script>
