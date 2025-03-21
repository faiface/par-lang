// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="introduction.html">Introduction</a></li><li class="chapter-item expanded "><a href="lexical.html"><strong aria-hidden="true">1.</strong> Lexical Structure</a></li><li class="chapter-item expanded "><a href="source-files.html"><strong aria-hidden="true">2.</strong> Source Files</a></li><li class="chapter-item expanded "><a href="items.html"><strong aria-hidden="true">3.</strong> Items</a></li><li class="chapter-item expanded "><a href="types.html"><strong aria-hidden="true">4.</strong> Types</a></li><li class="chapter-item expanded "><a href="expressions.html"><strong aria-hidden="true">5.</strong> Expressions</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="expressions/construction.html"><strong aria-hidden="true">5.1.</strong> Constructions</a></li><li class="chapter-item expanded "><a href="expressions/application.html"><strong aria-hidden="true">5.2.</strong> Applications</a></li></ol></li><li class="chapter-item expanded "><a href="patterns.html"><strong aria-hidden="true">6.</strong> Patterns</a></li><li class="chapter-item expanded "><a href="statements.html"><strong aria-hidden="true">7.</strong> Statements</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="statements/commands.html"><strong aria-hidden="true">7.1.</strong> Commands</a></li></ol></li><li class="chapter-item expanded "><a href="future.html"><strong aria-hidden="true">8.</strong> Future</a></li><li class="chapter-item expanded "><a href="appendix.html"><strong aria-hidden="true">9.</strong> Appendix</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="notation.html"><strong aria-hidden="true">9.1.</strong> Notation</a></li><li class="chapter-item expanded "><a href="glossary.html"><strong aria-hidden="true">9.2.</strong> Glossary</a></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
