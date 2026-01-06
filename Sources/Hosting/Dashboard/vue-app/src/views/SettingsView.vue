<template>
  <div class="max-w-3xl mx-auto p-8">
    <h1 class="text-3xl font-bold text-white mb-8">Global Configuration</h1>
    
    <!-- Paths -->
    <div class="bg-surface rounded-xl p-8 mb-8 border border-slate-700/50">
      <h2 class="text-lg font-semibold text-white mb-6 border-b border-white/5 pb-2">System Paths</h2>
      <div class="space-y-6">
        <div>
          <label class="block text-sm font-medium text-slate-400 mb-2">Dext CLI Path</label>
          <input type="text" v-model="config.dextPath" class="w-full px-4 py-2 rounded-lg bg-background border border-slate-700 text-white font-mono text-sm focus:border-secondary focus:outline-none transition-colors" readonly>
        </div>
        <div>
          <label class="block text-sm font-medium text-slate-400 mb-2">Code Coverage Path</label>
          <div class="flex gap-2">
             <input type="text" v-model="config.coveragePath" class="flex-1 px-4 py-2 rounded-lg bg-background border border-slate-700 text-white font-mono text-sm focus:border-secondary focus:outline-none transition-colors" placeholder="C:\Path\To\CodeCoverage.exe">
             <!-- Future: Install button impl -->
          </div>
        </div>
        <div class="pt-4 flex justify-end">
             <button @click="save" class="px-6 py-2 bg-secondary/80 hover:bg-secondary rounded-lg text-white font-medium transition-colors">Save Changes</button>
        </div>
      </div>
    </div>

    <!-- Environments -->
    <div class="bg-surface rounded-xl p-8 border border-slate-700/50">
      <div class="flex justify-between items-center mb-6 border-b border-white/5 pb-2">
         <div>
           <h2 class="text-lg font-semibold text-white">Delphi Environments</h2>
           <p class="text-xs text-slate-500 font-mono mt-1">{{ config.configPath }}</p>
         </div>
         <button @click="scan" :disabled="scanning" class="text-xs bg-slate-700 hover:bg-slate-600 px-3 py-1.5 rounded transition-colors border border-slate-600 text-slate-200">
            {{ scanning ? 'Scanning...' : 'Scan Now' }}
         </button>
      </div>

      <div class="space-y-4">
          <div v-for="env in config.environments" :key="env.version" class="p-4 rounded-lg border border-slate-700 bg-background/50 flex flex-col gap-2 hover:border-slate-600 transition-colors">
              <div class="flex justify-between items-center">
                  <div class="flex items-center gap-2">
                     <span class="font-semibold text-white text-sm">{{ env.name }}</span>
                     <span class="text-xs text-slate-500 bg-surface px-1.5 rounded">v{{ env.version }}</span>
                  </div>
                  <span v-if="env.isDefault" class="text-[10px] bg-secondary/20 text-secondary border border-secondary/30 px-2 py-0.5 rounded font-bold uppercase tracking-wider">Default</span>
                  <!-- Future: Set default button -->
              </div>
              <div class="text-[11px] text-slate-400 font-mono break-all bg-black/20 p-1.5 rounded border border-white/5">{{ env.path }}</div>
              <div class="flex flex-wrap gap-1.5 mt-1">
                  <span v-for="p in env.platforms" :key="p" class="text-[10px] bg-slate-700 px-1.5 py-0.5 rounded text-slate-300 border border-slate-600">{{ p }}</span>
              </div>
          </div>
          
          <div v-if="config.environments.length === 0" class="text-center py-8 text-slate-500 bg-surface/20 rounded-lg border border-dashed border-slate-700">
             <p>No Delphi installations found.</p>
             <p class="text-xs mt-1">Try clicking "Scan Now"</p>
          </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue';
import { fetchConfig, type Config } from '@/services/api';

const config = ref<Config>({ dextPath: '', coveragePath: '', configPath: '', environments: [] });
const scanning = ref(false);

async function load() {
  try {
     config.value = await fetchConfig();
  } catch(e) { console.error(e); }
}

async function save() {
    // Call API save (mock for now in prototype)
    alert('Config saved!');
}

async function scan() {
    scanning.value = true;
    setTimeout(() => { scanning.value = false; load(); }, 1000); 
    // In real app, call /api/env/scan
}

onMounted(() => load());
</script>
