(function () {
  let reveal = null;
  function update() {
    const grid = document.getElementById('grid');
    if (!grid) return;
    grid.classList.toggle('show-highlights', document.getElementById('show_highlights').checked);
    const sequences = grid.querySelector('.coin-sequences');
    if (!sequences || !reveal || Number(sequences.dataset.generation) !== reveal.generation) return;
    sequences.querySelectorAll('.flip-col-title').forEach(function (title, index) {
      const human = index + 1 === reveal.human;
      title.classList.toggle('reveal-human', human);
      title.classList.toggle('reveal-real', !human);
      const badge = title.querySelector('.badge');
      badge.classList.toggle('human', human);
      badge.textContent = human ? 'Human (pretending)' : 'Real flips';
      badge.style.display = 'inline-block';
    });
  }
  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('coin-reveal', function (message) { reveal = message; update(); });
    new MutationObserver(function (changes) {
      if (changes.some(change => Array.from(change.addedNodes).some(node => node.nodeType === 1))) update();
    }).observe(document.getElementById('grid'), {childList: true});
    update();
  });
  $(document).on('change', '#show_highlights', update);
})();
