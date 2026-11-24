/* Local wordcloud2.js integration. Answer text is never interpreted as HTML. */
(function () {
  'use strict';
  const states = new WeakMap();
  const observed = new Set();
  const palette = ['#1f497d', '#286ca5', '#654b84', '#526b2d', '#a34c38'];

  function render(host) {
    const width = Math.floor(host.clientWidth);
    if (width < 100 || !window.WordCloud) return;
    const key = width + ':' + host.dataset.words;
    if (states.get(host)?.key === key) return;
    const words = JSON.parse(host.dataset.words);
    if (!words.length) return;
    const canvas = host.querySelector('canvas');
    const status = host.querySelector('.response-cloud-status');
    const state = { key, attempt: 0, drawn: 0 };
    states.set(host, state);
    const max = Math.max(...words.map(item => item[1]));
    const fontSize = count => max === 1 ? 28 : 16 + 58 * Math.sqrt((count - 1) / (max - 1));
    const measure = document.createElement('canvas').getContext('2d');
    const area = words.reduce((total, item) => {
      const size = fontSize(item[1]);
      measure.font = '600 ' + size + 'px Arial';
      return total + measure.measureText(item[0]).width * size * 1.25;
    }, 0);
    let height = Math.max(420, Math.ceil(words.length / 40) * 140 + 260);
    let scale = Math.min(1, Math.sqrt(width * height * 0.6 / area));
    const ratio = Math.min(window.devicePixelRatio || 1, 2);

    function run() {
      if (!host.isConnected || states.get(host) !== state) return;
      state.drawn = 0;
      canvas.width = Math.round(width * ratio);
      canvas.height = Math.round(height * ratio);
      canvas.style.height = height + 'px';
      host.dataset.cloudReady = 'false';
      const drawn = event => { if (event.detail.drawn) state.drawn += 1; };
      const stopped = () => {
        canvas.removeEventListener('wordclouddrawn', drawn);
        canvas.removeEventListener('wordcloudstop', stopped);
        if (!host.isConnected || states.get(host) !== state) return;
        if (state.drawn < words.length && state.attempt < 7) {
          state.attempt += 1;
          scale *= 0.9;
          if (state.attempt > 3) height = Math.ceil(height * 1.15);
          setTimeout(run, 0);
          return;
        }
        host.dataset.cloudReady = 'true';
        host.dataset.drawnWords = String(state.drawn);
        host.dataset.totalWords = String(words.length);
        status.textContent = state.drawn === words.length
          ? 'All ' + words.length + ' distinct words shown. Hover over a word for its count.'
          : state.drawn + ' of ' + words.length + ' words fit; the complete counts are listed below.';
      };
      canvas.addEventListener('wordclouddrawn', drawn);
      canvas.addEventListener('wordcloudstop', stopped);
      WordCloud(canvas, {
        list: words.map(item => item.slice()),
        fontFamily: 'Arial, sans-serif', fontWeight: '600',
        weightFactor: count => fontSize(count) * scale * ratio,
        gridSize: Math.max(3, Math.round(4 * ratio)),
        color: word => palette[Array.from(word).reduce((n, c) => n + c.charCodeAt(0), 0) % palette.length],
        backgroundColor: '#ffffff', shape: 'circle', ellipticity: 0.72,
        shuffle: false, rotateRatio: 0.2, minRotation: -Math.PI / 2, maxRotation: 0,
        rotationSteps: 2, drawOutOfBound: false, shrinkToFit: false,
        abortThreshold: 0, wait: 0,
        hover: item => { if (item) status.textContent = item[0] + ': ' + item[1] + ' occurrences'; }
      });
    }
    run();
  }

  const sizes = new ResizeObserver(entries => entries.forEach(entry => render(entry.target)));
  function discover() {
    observed.forEach(host => {
      if (!host.isConnected) { sizes.unobserve(host); observed.delete(host); }
    });
    document.querySelectorAll('.response-cloud').forEach(host => {
      if (!observed.has(host)) { sizes.observe(host); observed.add(host); }
      render(host);
    });
  }
  let scheduled = false;
  function schedule() {
    if (scheduled) return;
    scheduled = true;
    requestAnimationFrame(() => { scheduled = false; discover(); });
  }
  document.addEventListener('DOMContentLoaded', () => {
    new MutationObserver(schedule).observe(document.body, { childList: true, subtree: true });
    discover();
  });
})();
