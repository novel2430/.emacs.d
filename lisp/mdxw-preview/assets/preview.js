(() => {
	const contentEl = document.getElementById("content");
	const btnTheme = document.getElementById("btn-theme");
	const btnPdf = document.getElementById("btn-pdf");

	const mdLight = document.getElementById("md-light");
	const mdDark = document.getElementById("md-dark");
	const hlLight = document.getElementById("hljs-light");
	const hlDark = document.getElementById("hljs-dark");

	// ---- theme ----
	function applyTheme(theme) {
		const t = (theme === "dark") ? "dark" : "light";

		// basic.css expects .dark-mode on a container (it also has .preview.dark-mode rules)
		document.body.classList.toggle("dark-mode", t === "dark");

		// markdown css
		if (mdLight) mdLight.disabled = (t === "dark");
		if (mdDark) mdDark.disabled = (t !== "dark");

		// highlight css
		if (hlLight) hlLight.disabled = (t === "dark");
		if (hlDark) hlDark.disabled = (t !== "dark");

		updateThemeButtonLabel(t);

		try { localStorage.setItem("mdxw-theme", t); } catch (e) { }
	}
	function updateThemeButtonLabel(theme) {
	  if (!btnTheme) return;
	  if (theme === "dark") {
		btnTheme.textContent = "🌙 Dark";
		btnTheme.title = "Switch to light theme";
		btnTheme.setAttribute("aria-label", "Switch to light theme");
	  } else {
		btnTheme.textContent = "☀️ Light";
		btnTheme.title = "Switch to dark theme";
		btnTheme.setAttribute("aria-label", "Switch to dark theme");
	  }
	}


	function loadTheme() {
		let t = "light";
		try {
			const saved = localStorage.getItem("mdxw-theme");
			if (saved === "dark" || saved === "light") t = saved;
		} catch (e) { }
		applyTheme(t);
	}

	window.setTheme = function(theme) { applyTheme(theme); };

	window.toggleTheme = function() {
		const curIsDark = document.body.classList.contains("dark-mode");
		applyTheme(curIsDark ? "light" : "dark");
	};


	// ---- init markdown-it ----
	function highlight(code, lang) {
		try {
			if (lang && window.hljs && hljs.getLanguage(lang)) {
				return hljs.highlight(code, { language: lang }).value;
			}
		} catch (e) { }
		return "";
	}

	const md = window.markdownit({
		html: true,
		linkify: true,
		typographer: true,
		highlight
	});

	// ---- attach data-line-start/end to block tokens (for scroll sync) ----
	md.core.ruler.after("block", "mdxw_line_map", (state) => {
		const tokens = state.tokens;
		for (const t of tokens) {
			if (t.map && t.map.length === 2) {
				const start = t.map[0] + 1; // 1-based
				const end1 = t.map[1];      // keep as-is
				t.attrSet("data-line-start", String(start));
				t.attrSet("data-line-end", String(end1));
			}
		}
	});

	// ---- fence override: wrap each code line with a span + mark it as code-line ----
	md.renderer.rules.fence = function(tokens, idx, options, env, self) {
		const token = tokens[idx];
		const info = (token.info || "").trim();
		const lang = info.split(/\s+/g)[0] || "";

		const startLine0 = (token.map && token.map[0] != null) ? token.map[0] : 0;
		const code = token.content || "";
		const codeNoTrail = code.replace(/\n$/, "");
		const _lines = codeNoTrail.length ? codeNoTrail.split("\n") : [""];

		let highlightedHtml = "";
		try {
			if (window.hljs && lang && hljs.getLanguage(lang)) {
				highlightedHtml = hljs.highlight(code, { language: lang }).value;
			} else if (window.hljs) {
				highlightedHtml = hljs.highlightAuto(code).value;
			} else {
				highlightedHtml = md.utils.escapeHtml(code);
			}
		} catch (e) {
			highlightedHtml = md.utils.escapeHtml(code);
		}

		const htmlNoTrail = highlightedHtml.replace(/\n$/, "");
		const htmlLines = htmlNoTrail.length ? htmlNoTrail.split("\n") : [""];

		const wrapped = htmlLines.map((lineHtml, i) => {
			const line1 = startLine0 + 1 + i; // 1-based
			return `<span data-mdxw-kind="code-line" data-line-start="${line1}" data-line-end="${line1}">${lineHtml}</span>`;
		}).join("\n");

		const preAttrs = [];
		if (token.map && token.map.length === 2) {
			preAttrs.push(`data-line-start="${token.map[0] + 1}"`);
			preAttrs.push(`data-line-end="${token.map[1]}"`);
		}
		const preAttrStr = preAttrs.length ? " " + preAttrs.join(" ") : "";

		const cls = lang ? ` class="language-${md.utils.escapeHtml(lang)}"` : "";
		return `<pre${preAttrStr}><code${cls}>${wrapped}</code></pre>\n`;
	};

	// ---- Emacs calls this ----
	window.updateContent = function(mdText) {
		if (!contentEl) return;
		const src = (mdText == null) ? "" : String(mdText);
		contentEl.innerHTML = md.render(src);
	};

	// ---- scroll sync (prefer code-line spans) ----
	window.synCursor = function(line) {
		const lineNum = Number(line);
		if (!Number.isFinite(lineNum) || lineNum <= 0) return;

		// Prefer code-line
		const codeNodes = document.querySelectorAll('[data-mdxw-kind="code-line"][data-line-start]');
		if (codeNodes && codeNodes.length > 0) {
			let best = null;
			let bestStart = -1;
			for (const el of codeNodes) {
				const s = Number(el.getAttribute("data-line-start"));
				const e = Number(el.getAttribute("data-line-end") ?? s);
				if (!Number.isFinite(s) || !Number.isFinite(e)) continue;
				if (s <= lineNum && lineNum <= e) {
					if (s >= bestStart) { best = el; bestStart = s; }
				}
			}
			if (best) {
				try { best.scrollIntoView({ behavior: "auto", block: "center", inline: "nearest" }); }
				catch (e) { best.scrollIntoView(true); }
				return;
			}
		}

		// Fallback
		const nodes = document.querySelectorAll("[data-line-start]");
		if (!nodes || nodes.length === 0) return;

		let best = null;
		let bestStart = -1;

		for (const el of nodes) {
			const s = Number(el.getAttribute("data-line-start"));
			const eAttr = el.getAttribute("data-line-end");
			const e = eAttr == null ? s : Number(eAttr);
			if (!Number.isFinite(s) || !Number.isFinite(e)) continue;

			if (s <= lineNum && lineNum <= e) {
				if (s >= bestStart) { best = el; bestStart = s; }
			}
		}

		if (!best) {
			for (const el of nodes) {
				const s = Number(el.getAttribute("data-line-start"));
				if (!Number.isFinite(s)) continue;
				if (s <= lineNum && s >= bestStart) { best = el; bestStart = s; }
			}
		}

		if (!best) return;
		try { best.scrollIntoView({ behavior: "auto", block: "center", inline: "nearest" }); }
		catch (e) { best.scrollIntoView(true); }
	};

	// ---- toolbar wiring ----
	if (btnTheme) btnTheme.addEventListener("click", () => window.toggleTheme());
	if (btnPdf) btnPdf.addEventListener("click", () => window.print());

	// load persisted theme once
	loadTheme();

	// ready flag (for debug)
	window.mdxwReady = true;
})();
