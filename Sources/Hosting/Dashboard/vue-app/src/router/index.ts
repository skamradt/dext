import { createRouter, createWebHashHistory } from 'vue-router'
import HomeView from '../views/HomeView.vue'
import TestsView from '../views/TestsView.vue'
import RunView from '../views/RunView.vue'
import SettingsView from '../views/SettingsView.vue'

const router = createRouter({
    history: createWebHashHistory(),
    routes: [
        {
            path: '/',
            name: 'home',
            component: HomeView
        },
        {
            path: '/tests',
            name: 'tests',
            component: TestsView
        },
        {
            path: '/run',
            name: 'run',
            component: RunView
        },
        {
            path: '/settings',
            name: 'settings',
            component: SettingsView
        }
    ]
})

export default router
