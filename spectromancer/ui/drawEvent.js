function drawEventsText(evs) {

  var swapped = false

  function text(x) {
    return $('<span/>').text(x)
  }

  function drawPlayer (who) {
    var p1 = text('Player 1')
    var p2 = text('Player 2')

    if (who === 'caster')
      return swapped? p2 : p1
    else
      return swapped? p1 : p2
  }

  function drawLoc(l) {
    return [ text('slot ' + l.slot + ' of '), drawPlayer(l.who) ]
  }


  function drawChange(x) {
    return (x>0)? text('gained ' + x).css('color','green')
                : text('lost ' + Math.abs(x)).css('color','red')
  }

  function drawElement(x) {
    var colors = { 'Water':   'cyan'
                 , 'Fire':    'orange'
                 , 'Air':     '#fff'
                 , 'Earth':   '#960'
                 , 'Special': '#99F'
                 }
    return text(x).css('color',colors[x])
  }

  function drawEv(ev) {
    var dom = $('<div/>')

    switch(ev.tag) {

      case 'say':
        return dom.append(text(ev.text))

      case 'life':
        return dom.append(text('The creature in '), drawLoc(ev.loc),
                          text(' '),drawChange(ev.amount), text(' life.'))

      case 'wizardLife':
        return dom.append(drawPlayer(ev.who), text(' '),
                            drawChange(ev.amount), text (' life.'))

      case 'die':
        return dom.append(text('The creature in '), drawLoc(ev.loc),
                                                            text(' died.'))

      case 'attack':
        return dom.append(text('The creature in '), drawLoc(ev.loc),
                                                          text(' attacked.'))
      case 'swap':
        swapped = !swapped
        return dom.append($('<hr/>'))

      case 'summon':
        return dom.append(text(ev.card.card.name), text(' summoned to '),
                          drawLoc(ev.loc) , text('.'))


      case 'move':
        return dom.append(text('The creature in '), drawLoc(ev.from),
                          text(' moved to '), drawLoc(ev.to))

      case 'doSomething':
        return dom.append(text('The creature in '), drawLoc(ev.loc),
                            text(' did something.'))

      case 'power':
        return dom.append(drawPlayer(ev.who), text(' '), drawChange(ev.amount),
                         text(' '), drawElement(ev.element), text(' power.'))

      default:
        return dom.append(text('(unknown event: ' + ev.tag + ')'))
    }
  }

  var box = $('<div/>')
            .css('text-align','left')
  jQuery.each(evs, function(ix,ev) {
    box.append(drawEv(ev))
  })

  return box
}


function drawEvents(evs,k) {

  var swapped = false

  var inProgress = 0
  var finished = false

  function getWizard(who) {
    if (swapped) {
      who = swapWizard(who)
    }

    return who
  }

  function swapWizard(who) {
    return who === 'caster' ? 'opponent' : 'caster'
  }

  function getWizardNameBox(who) {
    return $('#' + getWizard(who) + ' .name')
  }

  function getLoc(l) {
    return $('#' + getWizard(l.who) + '_' + l.slot)
  }

  function getOpp(l) {
    return $('#' + getWizard(swapWizard(l.who)) + "_" + l.slot)
  }


  function endAnimation() {
    --inProgress
    if (inProgress === 0 && finished) {
      setTimeout(k,0)
    }
  }

  function drawEvent(i) {
    if (i >= evs.length) {
      if (inProgress === 0)
        setTimeout(k,0)
      else finished = true
           return
    }

    function next() { drawEvent(i+1) }

    var ev = evs[i]
    switch(ev.tag) {

      case 'say':
        console.log(ev.text)
        setTimeout(next,0)
        return

      case 'life':
        if (ev.amount === 0) {
          setTimeout(next,0)
          return
        }
        var t = $(getLoc(ev.loc))
        var pos = t.offset()
        var msg = $('<div/>')
                  .css('position','absolute')
                  .css('left',pos.left + 'px')
                  .css('top', pos.top + 'px')
                  .css('display','inline-block')
                  .css('color', ev.amount >= 0 ? 'green' : 'red')
                  .css('background-color', 'rgba(0,0,0,0.5)')
                  .css('z-index','200')
                  .css('font-size', '40px')
                  .css('border-radius','3px')
                  .css('padding','5px')
                  .text(ev.amount)
         $('body').append(msg)
         changeSlotLife(getWizard(ev.loc.who), ev.loc.slot, ev.amount)
         msg.animate({ left: "-=20px", top: "-=20px", opacity: '0' }
                    , 1000
                    , 'swing'
                    , next
                    )
         return

      case 'wizardLife':
        var r = getWizard(ev.who)
        var lab = $('#' + r)
        var pos = lab.offset()
        var msg = $('<div/>')
                  .css('position','absolute')
                  .css('left',(pos.left + lab.width()/2) + 'px')
                  .css('top', pos.top + 'px')
                  .css('display','inline-block')
                  .css('color', ev.amount >= 0 ? 'green' : 'red')
                  .css('background-color', 'rgba(0,0,0,0.5)')
                  .css('z-index','200')
                  .css('font-size', '40px')
                  .css('border-radius','3px')
                  .css('padding','5px')
                  .text(ev.amount)
         $('body').append(msg)
         msg.animate({ left: "-=20px", top: "-=20px", opacity: '0' }
                    , 1000
                    , 'swing'
                    , next
                    )
         changeWizLife(r, ev.amount)
         return


      case 'die':
        $(getLoc(ev.loc)).fadeOut(next)
        return

      case 'attack':
        var ch1 = "+=50px"
        var ch2 = "-=50px"

        if(getLoc(ev.loc).hasClass('arena_right')) {
          var ch1 = "-=50px"
          var ch2 = "+=50px"
        }

        getLoc(ev.loc).css('z-index','100')
                      .animate({left: ch1})
                      .animate({left: ch2}, next)
        return

      case 'swap':
        swapped = !swapped
        var d = $('<div/>')
                .text('Next Turn')
                .css('position','absolute')
                .css('left', '30%')
                .css('top', '30%')
                .css('background-color','white')
                .css('color','black')
                .css('font-size','128px')
                .css('border','5px solid black')
                .css('z-index','500')

        $('body').append(d)
        d.animate({opacity: 0}, 1000, 'swing', function() {
          $('.deck.' + getWizard('caster')).removeClass('disabled')
          $('.deck.' + getWizard('opponent')).addClass('disabled')
          setTimeout(next,0)
        })

        return

      case 'summon':
        var n = drawArenaField(ev.card,getWizard(ev.loc.who),ev.loc.slot)
        getLoc(ev.loc).replaceWith(n)
        setTimeout(next,0)
        return

      case 'move':
        var tgt = getLoc(ev.to)
        var tgtPos = tgt.offset()
        var src = getLoc(ev.from).css('position','absolute')
        var id = tgt.attr('id')
        src.animate({left: tgtPos.left + 'px', top: tgtPos.top + 'px'},
            'fast','swing', function() {
            tgt.replaceWith(src)
            src.attr('id', id)
            next()
          })
        return

      case 'doSomething':
        getLoc(ev.loc).animate({top: '-=2px'},'fast')
                      .animate({top: '+=4px'},'fast')
                      .animate({top: '-=2px'},'fast',next)
        return

      case 'power':
        var r = getWizard(ev.who)
        var el = $('#' + r + '_' + ev.element)
        var loc = el.offset()
        var dom = $('<div/>')
                  .css('display','inline-block')
                  .css('position','absolute')
                  .css('left', loc.left)
                  .css('top', loc.top)
                  .css('color','white')
                  .text(ev.amount)
        $('body').append(dom)
        ++inProgress
        dom.animate( { left: '-=20px', top: '-=20px', opacity: 0 }
                   , 'slow', 'swing', endAnimation)
        changeWizPow(r,ev.element, ev.amount)
        setTimeout(next,0)
        return

      default:
        console.log('unknown event', ev)
        setTimeout(next,0)
        return
    }

  }

  drawEvent(0)
}

