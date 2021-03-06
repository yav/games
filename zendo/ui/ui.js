var rowNum = 5
var colNum = 5


function drawColorChooser(notify) {
  var dom = $('<div/>')
  function color(c,v) {
    var it = $('<div/>').addClass('box box-size')
                        .addClass(c)
                        .data('value',v)
    it.click(function () {
      dom.find('.selected').removeClass('selected')
      it.addClass('selected')
      notify()
    })
    dom.append(it)
  }

  color('red','red')
  color('green','green')
  color('blue','blue')
  color('cell-color',null)

  dom.children(':first-child').addClass('selected')
  return dom.addClass('editor')
}

var shapes = { circle: '&#9679;'
             , triangle: '&#9650;'
             , square: '&#9632;'
             }


function drawShapeChooser(notify) {
  var dom = $('<div/>')
  function shape(v) {
    var it = $('<div/>').addClass('box box-size')
                        .html(shapes[v])
                        .data('value',v)
    it.click(function () {
      dom.find('.selected').removeClass('selected')
      it.addClass('selected')
      notify()
    })
    dom.append(it)
  }

  shape('circle')
  shape('square')
  shape('triangle')
  dom.children(':first-child').addClass('selected')
  return dom.addClass('editor')
}



function drawEditor() {

  var dom = $('<div/>')

  var tab = $('<table/>').addClass('editor')
  var cols = drawColorChooser(notify)
  var shape = drawShapeChooser(notify)

  function notify() {
    jQuery.each(tab.find('.selected'), function(ix,thing) {
      var thing = $(thing)
      var loc = thing.data('loc')
      clicked(loc.row,loc.col,thing)()
    })
  }

  function clicked(row,col,thing) { return function() {
    tab.find('.selected').removeClass('selected')
    thing.addClass('selected')
    var c = cols.find('.selected').data('value')
    var s = shape.find('.selected').data('value')
    thing.empty()
    if (c === null) thing.data('value',null)
    else thing.html(shapes[s]).css('color',c)
              .data('value', { color: c, shape: s })
  }}

  for (var row  = 0; row < rowNum; ++row) {
    var tr = $('<tr/>')
    for (var col = 0; col < colNum; ++col) {
      var td = $('<td/>').addClass('box-size cell-color')
      td.click(clicked(row,col,td))
        .data('loc', { row: row, col: col })
      td.data('value',null)
      tr.append(td)
    }
    tab.append(tr)
  }

  function list() {
    var ex = {}
    jQuery.each(tab.find('td'), function(ix,td) {
      var td = $(td)
      var v = td.data('value')
      var l = td.data('loc')
      if (v !== null) ex[l.col + '-' + l.row] = v
    })
    $('body').append(drawExample(ex))
  }

  var btn = $('<div/>').addClass('box box-size')
                       .text('see')
                       .click(function() { list() })

  dom.append(cols,shape,tab,btn)

  return dom
}


function drawExample(ex) {
  var dom = $('<table/>').addClass('example')
  for (var row = 0; row < rowNum; ++row) {
    var tr = $('<tr/>').addClass('cell-color')
    for (var col = 0; col < colNum; ++col) {
      var td = $('<td/>')
      var v = ex[col + '-' + row]
      if (v) { td.html(shapes[v.shape])
                 .css('color',v.color) }
      tr.append(td)
    }
    dom.append(tr)
  }

  return dom
}


