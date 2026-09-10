document.addEventListener('DOMContentLoaded', function () {

    // Theme toggle: cycles system -> light -> dark -> system, persisted
    // per-browser via localStorage. Wrapped defensively since some
    // browsers (private-browsing mode) throw on storage access.
    (function () {
        var root = document.documentElement;
        var btn = document.getElementById('themeToggle');
        var label = document.getElementById('themeLabel');
        if (!btn || !label) return;

        var order = [null, 'light', 'dark'];
        var names = { 'null': 'Match system theme', light: 'Light theme', dark: 'Dark theme' };

        function apply(v) {
            if (v) { root.setAttribute('data-theme', v); } else { root.removeAttribute('data-theme'); }
            label.textContent = names[v];
            try { localStorage.setItem('rpki-prover-theme', v || ''); } catch (e) { /* ignore */ }
        }

        var current = null;
        try {
            var stored = localStorage.getItem('rpki-prover-theme');
            if (stored) current = stored;
        } catch (e) { /* ignore */ }
        apply(current);

        btn.addEventListener('click', function () {
            current = order[(order.indexOf(current) + 1) % order.length];
            apply(current);
        });
    })();

    // Expand/collapse fetch detail rows, with a rotating chevron.
    // Clicking the repository link itself navigates instead of toggling.
    document.querySelectorAll('.clickable-row').forEach(function (row) {
        row.addEventListener('click', function (e) {
            if (e.target.closest('a')) return;
            var targetId = row.getAttribute('data-target');
            var detailRow = targetId && document.getElementById(targetId);
            if (!detailRow) return;
            var willShow = detailRow.style.display === 'none' || detailRow.style.display === '';
            detailRow.style.display = willShow ? 'table-row' : 'none';
            row.classList.toggle('open', willShow);
        });
    });

    // Per-table filter boxes: substring match against each row's own text.
    function wireFilter(inputId, tableId) {
        var input = document.getElementById(inputId);
        var table = document.getElementById(tableId);
        if (!input || !table) return;
        input.addEventListener('input', function () {
            var q = input.value.toLowerCase();
            table.querySelectorAll('tbody > tr').forEach(function (row) {
                if (row.classList.contains('detail-row')) return;
                var show = row.textContent.toLowerCase().indexOf(q) !== -1;
                row.style.display = show ? '' : 'none';
                var targetId = row.getAttribute('data-target');
                var detail = targetId && document.getElementById(targetId);
                if (detail) {
                    detail.style.display = (show && row.classList.contains('open')) ? 'table-row' : 'none';
                }
            });
        });
    }
    wireFilter('rrdpFilter', 'rrdpTable');
    wireFilter('rsyncFilter', 'rsyncTable');

    function openAncestorDetails(el) {
        for (var node = el; node; node = node.parentElement) {
            if (node.tagName === 'DETAILS' && !node.open) node.open = true;
        }
    }

    // Marks the issue an anchor currently points at, persistently --
    // no timer to fade it before it's been seen. Stays until a
    // different issue becomes the linked one, or the fragment clears.
    var linkedIssue = null;
    function setLinked(el) {
        if (linkedIssue && linkedIssue !== el) linkedIssue.classList.remove('is-linked');
        if (el) el.classList.add('is-linked');
        linkedIssue = el;
    }

    // Deep-linking to a single issue. A #issue-... fragment can point
    // inside a collapsed TA group and/or a collapsed object chain, and
    // plain browser navigation won't open a closed <details> on its
    // own -- only Ctrl+F does that -- so open every <details> ancestor
    // by hand. The issue row also carries its own collapsed "N more
    // hops to the TA" path-chain as a *descendant* <details>, which a
    // shared link should land already expanded, not behind a click.
    function revealTarget(id) {
        if (!id) return;
        var el = document.getElementById(id);
        if (!el) return;
        openAncestorDetails(el);
        el.querySelectorAll('details').forEach(function (d) { d.open = true; });
        el.scrollIntoView({ block: 'center' });
        setLinked(el);
    }
    if (location.hash) {
        window.setTimeout(function () { revealTarget(location.hash.slice(1)); }, 30);
    }
    window.addEventListener('hashchange', function () {
        var id = location.hash.slice(1);
        if (id) revealTarget(id); else setLinked(null);
    });

    // Copy-link buttons on individual issues.
    function copyText(text) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
            return navigator.clipboard.writeText(text).catch(function () { return legacyCopy(text); });
        }
        return legacyCopy(text);
    }
    function legacyCopy(text) {
        return new Promise(function (resolve) {
            var ta = document.createElement('textarea');
            ta.value = text;
            ta.style.position = 'fixed';
            ta.style.opacity = '0';
            document.body.appendChild(ta);
            ta.focus();
            ta.select();
            try { document.execCommand('copy'); } catch (e) { /* best effort */ }
            document.body.removeChild(ta);
            resolve();
        });
    }
    document.querySelectorAll('.copy-link').forEach(function (btn) {
        btn.addEventListener('click', function (e) {
            e.stopPropagation();
            var id = btn.getAttribute('data-anchor');
            if (!id) return;
            var url = location.origin + location.pathname + location.search + '#' + id;
            history.pushState(null, '', '#' + id);
            // The issue is already on screen -- its own button was just
            // clicked -- so just mark it as linked; no need to reopen
            // ancestors or scroll, which would yank the page around.
            var el = document.getElementById(id);
            if (el) setLinked(el);
            copyText(url).then(function () {
                btn.classList.add('copied');
                window.setTimeout(function () { btn.classList.remove('copied'); }, 1200);
            });
        });
    });

    // Sidebar scroll-spy: highlight the nav link for whichever section
    // is currently in view.
    var sectionIds = ['validation-metrics', 'validation-issues', 'general-issues', 'rrdp-fetches', 'rsync-fetches'];
    var sections = sectionIds.map(function (id) { return document.getElementById(id); }).filter(Boolean);
    var navLinks = document.querySelectorAll('.side-link[href^="#"]');
    if ('IntersectionObserver' in window && sections.length && navLinks.length) {
        var observer = new IntersectionObserver(function (entries) {
            entries.forEach(function (entry) {
                if (!entry.isIntersecting) return;
                navLinks.forEach(function (link) {
                    link.classList.toggle('active', link.getAttribute('href') === '#' + entry.target.id);
                });
            });
        }, { rootMargin: '-10% 0px -70% 0px' });
        sections.forEach(function (section) { observer.observe(section); });
    }
});
