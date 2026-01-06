<template>
  <div class="min-h-screen bg-background text-text font-sans">
    <Sidebar />
    <main class="ml-16 min-h-screen">
      <router-view v-slot="{ Component }">
        <transition name="fade" mode="out-in">
          <component :is="Component" />
        </transition>
      </router-view>
    </main>
  </div>
</template>

<script setup lang="ts">
import { onMounted } from 'vue';
import Sidebar from './components/layout/Sidebar.vue';
import { useHubStore } from '@/stores/hubStore';

const hubStore = useHubStore();

onMounted(() => {
  hubStore.initialize();
});
</script>

<style>
.fade-enter-active,
.fade-leave-active {
  transition: opacity 0.2s ease;
}

.fade-enter-from,
.fade-leave-to {
  opacity: 0;
}
</style>
