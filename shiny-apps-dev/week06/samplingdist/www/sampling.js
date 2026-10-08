(function () {
  'use strict';
  let latest = null;
  const blue = '#6FA8DC', red = '#B35A5A', pop = '#D34E4E';
  function rows(data) {
    if (Array.isArray(data)) return data;
    if (!data || !data.mean) return [];
    return data.mean.map((_, i) => Object.fromEntries(Object.keys(data).map(k => [k, data[k][i]])));
  }
  function num(x) { return Number(x).toFixed(3).replace(/\.?0+$/, ''); }
  function graph(id, domain, ymax, ylabel, title) {
    const canvas = document.getElementById(id), width = canvas.clientWidth, height = canvas.clientHeight;
    if (!width) return null;
    const ratio = Math.min(window.devicePixelRatio || 1, 2);
    canvas.width = width * ratio; canvas.height = height * ratio;
    const ctx = canvas.getContext('2d'); ctx.scale(ratio, ratio);
    const left = 48, right = width - 15, top = 32, bottom = height - 40;
    const x = v => left + (v-domain[0])/(domain[1]-domain[0])*(right-left);
    const y = v => bottom - v/ymax*(bottom-top);
    ctx.font = '12px sans-serif'; ctx.fillStyle = '#333';
    ctx.strokeStyle = '#aaa';ctx.lineWidth=1;ctx.strokeRect(left,top,right-left,bottom-top);
    for (let i=0;i<=4;i++) {
      const v=domain[0]+(domain[1]-domain[0])*i/4;
      ctx.textAlign='center';ctx.fillText(num(v),x(v),bottom+17);
      if (ylabel) {ctx.textAlign='right';ctx.fillText(num(ymax*i/4),left-6,y(ymax*i/4)+4);}
    }
    ctx.textAlign='center';ctx.fillText(ylabel ? 'Sample mean' : 'Value', (left+right)/2,height-5);
    ctx.fillText(title,(left+right)/2,17);
    if(ylabel) {ctx.save();ctx.translate(12,(top+bottom)/2);ctx.rotate(-Math.PI/2);ctx.fillText(ylabel,0,0);ctx.restore();}
    function line(x1,y1,x2,y2,color,weight=2) {
      ctx.strokeStyle=color;ctx.lineWidth=weight;ctx.beginPath();ctx.moveTo(x(x1),y(y1));ctx.lineTo(x(x2),y(y2));ctx.stroke();
    }
    function point(v,z,color) {ctx.fillStyle=color;ctx.beginPath();ctx.arc(x(v),y(z),4,0,2*Math.PI);ctx.fill();}
    return {canvas,ctx,x,y,line,point,top,bottom};
  }
  function draw(message, acknowledge) {
    const samples=rows(message.samples), current=message.current;
    const domain=message.xlim.slice();
    // Include rare outlying means and interval endpoints instead of dropping observations.
    samples.forEach(s=>{domain[0]=Math.min(domain[0],s.low);domain[1]=Math.max(domain[1],s.high);});
    const step=message.binwidth, lo=Math.floor(domain[0]/step)*step, hi=Math.ceil(domain[1]/step)*step;
    domain[0]=lo;domain[1]=Math.max(lo+step,hi);
    const counts=Array(Math.ceil((domain[1]-lo)/step)).fill(0);
    samples.forEach(s=>{const i=Math.min(counts.length-1,Math.max(0,Math.floor((s.mean-lo)/step)));counts[i]++;});
    const ymax=Math.max(10,...counts)+2;
    const hist=graph('hist_plot',domain,ymax,'Count','Gray curve: theoretical sampling distribution');
    if(hist) {
      hist.ctx.fillStyle=blue;
      counts.forEach((count,i)=>{if(count)hist.ctx.fillRect(hist.x(lo+i*step),hist.y(count),Math.max(1,hist.x(lo+(i+1)*step)-hist.x(lo+i*step)-1),hist.y(0)-hist.y(count));});
      hist.line(message.mu,0,message.mu,ymax,pop);
      const sd=message.sigma/Math.sqrt(message.n);
      hist.ctx.strokeStyle='#666';hist.ctx.lineWidth=2;hist.ctx.beginPath();
      for(let i=0;i<=200;i++){const v=lo+(domain[1]-lo)*i/200,z=Math.exp(-.5*((v-message.mu)/sd)**2)*ymax*.9;
        if(i)hist.ctx.lineTo(hist.x(v),hist.y(z));else hist.ctx.moveTo(hist.x(v),hist.y(z));}
      hist.ctx.stroke();hist.canvas.dataset.sampleCount=samples.length;
      hist.canvas.setAttribute('aria-label','Sampling distribution: '+samples.length+' sample means');
    }
    const dot=graph('current_mean_dot_plot',domain,1,null,'Current sample mean (point estimate)');
    const whisker=graph('current_ci_whisker_plot',domain,1,null,'Current confidence interval (same scale)');
    [dot,whisker].forEach(g=>{if(g)g.line(message.mu,0,message.mu,1,pop);});
    if(current){if(dot)dot.point(current.mean,.5,blue);if(whisker){const color=current.low<=message.mu&&message.mu<=current.high?blue:red;whisker.line(current.low,.5,current.high,.5,color,3);whisker.point(current.mean,.5,color);}}
    const shown=samples.slice(-100), minimum=Math.max(1,message.index-99), maximum=Math.max(100,message.index);
    const intervals=graph('ci_plot',domain,maximum-minimum+4,'Sample #','Confidence interval: '+Math.round(message.conf*100)+'%');
    if(intervals){
      intervals.line(message.mu,0,message.mu,maximum-minimum+4,pop);
      // CI labels use the actual sample number, even when the most recent 100 scroll.
      intervals.ctx.fillStyle='#fff';intervals.ctx.fillRect(20,intervals.top-7,25,intervals.bottom-intervals.top+20);
      intervals.ctx.fillStyle='#333';intervals.ctx.textAlign='right';
      for(let i=0;i<=4;i++){const z=(maximum-minimum)*i/4+2;intervals.ctx.fillText(num(minimum+(maximum-minimum)*i/4),42,intervals.y(z)+4);}
      shown.forEach(s=>{const color=s.contains_mu?blue:red,z=s.sample_id-minimum+2;intervals.line(s.low,z,s.high,z,color);intervals.point(s.mean,z,color);});
      intervals.canvas.dataset.sampleCount=samples.length;
    }
    document.getElementById('status_box').textContent='Step: '+message.index+' / 100 | n: '+message.n+' | CI: '+Math.round(message.conf*100)+'%'+(message.running?' | Running':'');
    document.getElementById('add1').disabled=message.running;
    document.getElementById('current_sample_text').textContent=message.values.length?message.values.map(num).join(', '):'Add a sample to display the values.';
    document.getElementById('ci_error_text').textContent=samples.length?'CI errors: '+samples.filter(s=>!s.contains_mu).length:'';
    document.getElementById('current_stats_tbl').textContent=current?'n: '+current.n+' | Mean: '+num(current.mean)+' | SD: '+num(current.sd)+' | SE: '+num(current.se):'';
    document.getElementById('formula_panel').innerHTML=current?
      '<div class="formula-columns"><section><strong>Formulas</strong><p>SE = s / &radic;n</p><p>x&#772; &plusmn; t* &times; SE</p></section><section><strong>Plug in numbers</strong><p>SE = '+num(current.sd)+' / &radic;'+current.n+'</p><p>t* = t quantile at '+num((1+current.conf)/2)+', df = '+(current.n-1)+'</p><p>CI = '+num(current.mean)+' &plusmn; '+num(current.tcrit)+' &times; '+num(current.se)+'</p></section><section><strong>Results</strong><p>SE = '+num(current.se)+'</p><p>t* = '+num(current.tcrit)+'</p><p>CI = ['+num(current.low)+', '+num(current.high)+']</p></section></div>':'Add a sample to show formulas.';
    if(acknowledge)requestAnimationFrame(()=>Shiny.setInputValue('sample_drawn',{generation:message.generation,index:message.index},{priority:'event'}));
  }
  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('sampling-frame', function (message) {latest=message;draw(message,true);});
    const observer=new ResizeObserver(()=>{if(latest)draw(latest,false);});
    document.querySelectorAll('.sample-chart').forEach(canvas=>observer.observe(canvas));
  });
})();
