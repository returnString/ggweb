const ggWebOB = new Shiny.OutputBinding();

ggWebOB.find = function(scope) {
  return $(scope).find('div.shiny-ggweb-output');
};

const ggWebSet = {};

function ggWebPrettify(prefix, val) {
 const asNum = parseFloat(val);
  if (!isNaN(asNum)) {
    val = asNum.toLocaleString();
  }

  if (prefix) {
    val = prefix + ': ' + val;
  }

  return val;
}

ggWebOB.renderValue = function(divEl, data) {
  const prevCharts = ggWebSet[divEl.id];
  if (prevCharts) {
    for (const chart of prevCharts) {
      chart.destroy();
    }
  }

  while (divEl.hasChildNodes()) {
    divEl.removeChild(divEl.lastChild);
  }

  const payload = JSON.parse(data);

  if (payload.title) {
    const title = document.createElement('h4');
    title.textContent = payload.title;
    title.style['text-align'] = 'center';
    divEl.appendChild(title);
  }

  // FIXME: this is all atrocious, need to learn web layouting

  const panelContainer = document.createElement('div');
  panelContainer.style.height = '90%';

  const charts = [];
  for (const chartSpec of payload.panels) {
    const chartContainer = document.createElement('div');
    chartContainer.style.position = 'relative';
    chartContainer.style.display = 'inline-block';
    chartContainer.style.width = (100 / payload.panelLayout.columns) + '%';
    chartContainer.style.height = (100 / payload.panelLayout.rows) + '%';
    chartContainer.setAttribute('class', 'chart-container');
    const canvas = document.createElement('canvas');
    chartContainer.appendChild(canvas);
    panelContainer.appendChild(chartContainer);

    chartSpec.options.tooltips.callbacks = {
      label: (tooltipItem, data) => ggWebPrettify(data.datasets[tooltipItem.datasetIndex].label, tooltipItem.yLabel),
    };

    chartSpec.options.scales.yAxes[0].ticks.callback = (val) => ggWebPrettify('', val);

    const ctx = canvas.getContext('2d');
    const chart = new Chart(ctx, chartSpec);
    charts.push(chart);
  }

  divEl.appendChild(panelContainer);
  ggWebSet[divEl.id] = charts;
};

Shiny.outputBindings.register(ggWebOB, 'ggWebOB');
