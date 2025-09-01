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
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="introduction.html">Introduction</a></li><li class="chapter-item expanded "><a href="getting_started.html"><strong aria-hidden="true">1.</strong> Getting Started</a></li><li class="chapter-item expanded "><a href="basic_program_structure.html"><strong aria-hidden="true">2.</strong> Basic Program Structure</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="structure/definitions_and_declarations.html"><strong aria-hidden="true">2.1.</strong> Definitions &amp; Declarations</a></li><li class="chapter-item expanded "><a href="structure/primitive_types.html"><strong aria-hidden="true">2.2.</strong> Primitive Types</a></li><li class="chapter-item expanded "><a href="structure/let_expressions.html"><strong aria-hidden="true">2.3.</strong> The let Expression</a></li></ol></li><li class="chapter-item expanded "><a href="big_table.html"><strong aria-hidden="true">3.</strong> The Big Table</a></li><li class="chapter-item expanded "><a href="builtin.html"><strong aria-hidden="true">4.</strong> Built-In Definitions</a></li><li class="chapter-item expanded "><a href="types_and_expressions.html"><strong aria-hidden="true">5.</strong> Types &amp; Their Expressions</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="types/unit.html"><strong aria-hidden="true">5.1.</strong> Unit</a></li><li class="chapter-item expanded "><a href="types/either.html"><strong aria-hidden="true">5.2.</strong> Either</a></li><li class="chapter-item expanded "><a href="types/pair.html"><strong aria-hidden="true">5.3.</strong> Pair</a></li><li class="chapter-item expanded "><a href="types/function.html"><strong aria-hidden="true">5.4.</strong> Function</a></li><li class="chapter-item expanded "><a href="types/forall.html"><strong aria-hidden="true">5.5.</strong> Forall</a></li><li class="chapter-item expanded "><a href="types/recursive.html"><strong aria-hidden="true">5.6.</strong> Recursive</a></li><li class="chapter-item expanded "><a href="types/choice.html"><strong aria-hidden="true">5.7.</strong> Choice</a></li><li class="chapter-item expanded "><a href="types/iterative.html"><strong aria-hidden="true">5.8.</strong> Iterative</a></li><li class="chapter-item expanded "><a href="types/box.html"><strong aria-hidden="true">5.9.</strong> Box</a></li><li class="chapter-item expanded "><a href="types/exists.html"><strong aria-hidden="true">5.10.</strong> Exists</a></li><li class="chapter-item expanded "><a href="types/continuation.html"><strong aria-hidden="true">5.11.</strong> Continuation</a></li></ol></li><li class="chapter-item expanded "><a href="process_syntax.html"><strong aria-hidden="true">6.</strong> The Process Syntax</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="processes/do_expression.html"><strong aria-hidden="true">6.1.</strong> The do Expression</a></li><li class="chapter-item expanded "><a href="processes/commands.html"><strong aria-hidden="true">6.2.</strong> Commands</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="processes/commands/selecting_and_sending.html"><strong aria-hidden="true">6.2.1.</strong> Selecting &amp; Sending</a></li><li class="chapter-item expanded "><a href="processes/commands/looping_and_branching.html"><strong aria-hidden="true">6.2.2.</strong> Looping &amp; Branching</a></li><li class="chapter-item expanded "><a href="processes/commands/receiving_where_it_shines.html"><strong aria-hidden="true">6.2.3.</strong> Receiving, Where It Shines</a></li></ol></li><li class="chapter-item expanded "><a href="processes/chan_expression.html"><strong aria-hidden="true">6.3.</strong> Channels &amp; Linking</a></li><li class="chapter-item expanded "><a href="processes/duality.html"><strong aria-hidden="true">6.4.</strong> Construction by Destruction</a></li></ol></li><li class="chapter-item expanded "><a href="error_handling.html"><strong aria-hidden="true">7.</strong> Error Handling</a></li></ol>';
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
