// Client-Side Logic for Dext Documentation
document.addEventListener('DOMContentLoaded', () => {
    
    // 1. Theme Toggling
    const themeToggle = document.getElementById('themeToggle');
    const storedTheme = localStorage.getItem('dext-theme') || 'dark';
    document.documentElement.setAttribute('data-theme', storedTheme);

    themeToggle.addEventListener('click', () => {
        const currentTheme = document.documentElement.getAttribute('data-theme');
        const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
        
        document.documentElement.setAttribute('data-theme', newTheme);
        localStorage.setItem('dext-theme', newTheme);
        
        // Re-render Mermaid if needed (optional)
        // mermaid.init(); 
    });

    // 2. Search Functionality
    const searchInput = document.getElementById('searchInput');
    const navItems = document.querySelectorAll('.nav-item');

    searchInput.addEventListener('input', (e) => {
        const term = e.target.value.toLowerCase();
        
        navItems.forEach(item => {
            const text = item.textContent.toLowerCase();
            if(text.includes(term)) {
                item.style.display = 'block';
            } else {
                item.style.display = 'none';
            }
        });
    });

    // 3. Initialize Mermaid Manually to fix SVG sizing
    mermaid.initialize({ 
        startOnLoad: false, 
        "class": { useMaxWidth: false },
        theme: document.documentElement.getAttribute('data-theme') === 'dark' ? 'dark' : 'default' 
    });

    mermaid.run().then(() => {
        // Post-processing: remove width="100%" and set explicit width/height based on viewBox
        document.querySelectorAll('.mermaid-container svg').forEach(svg => {
             const viewBox = svg.getAttribute("viewBox");
             if (viewBox) {
                 const parts = viewBox.split(" ");
                 const width = parts[2];
                 const height = parts[3];
                 svg.style.width = width + "px";
                 svg.style.height = height + "px";
             }
             svg.style.maxWidth = "none";
             svg.removeAttribute("width");
        });
        
        // Make visible again
        document.querySelectorAll('.mermaid').forEach(el => el.style.visibility = 'visible');
    });
});