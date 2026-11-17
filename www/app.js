// Rinse client helpers: remember sign-in, idle sign-out, mobile menu, Enter-to-submit, install as an app.
(function(){
  var KEY='rinse_token', OLD='sift_token', OLD2='vyrel_token', idleMs=0, timer=null;
  try{['rinse_theme','sift_theme','vyrel_theme'].forEach(function(k){localStorage.removeItem(k);});}catch(e){}
  function readTok(){try{return localStorage.getItem(KEY)||localStorage.getItem(OLD)||localStorage.getItem(OLD2)||'';}catch(e){return '';}}
  document.addEventListener('shiny:connected',function(){
    var tok=readTok();
    Shiny.setInputValue('vy_token',tok,{priority:'event'});
  });
  function resetIdle(){ if(!idleMs)return; clearTimeout(timer); timer=setTimeout(function(){Shiny.setInputValue('idle',Date.now(),{priority:'event'});},idleMs); }
  ['click','keydown','touchstart','mousemove','scroll'].forEach(function(e){document.addEventListener(e,resetIdle,{passive:true});});
  if(window.Shiny){
    Shiny.addCustomMessageHandler('vy_store',function(t){try{t?localStorage.setItem(KEY,t):localStorage.removeItem(KEY);}catch(e){}});
    Shiny.addCustomMessageHandler('vy_scroll',function(_){window.scrollTo({top:0,behavior:'smooth'});});
    Shiny.addCustomMessageHandler('vy_cfg',function(c){idleMs=(c.idle||0)*60000;resetIdle();});
    Shiny.addCustomMessageHandler('vy_sheet',function(_){var s=document.querySelector('.sheet');if(s)s.classList.remove('open');});
  }
  document.addEventListener('keydown',function(e){
    if(e.key!=='Enter'||e.target.tagName!=='INPUT')return;
    var f=e.target.closest('.form'); if(!f)return;
    var b=f.querySelector('.btn.primary'); if(b){e.preventDefault(); setTimeout(function(){b.click();},50);}
  });
  /* Tell browsers and password managers what each sign-in field is, so saved details can be filled in. */
  var HINT={li_email:'username',li_pw:'current-password',su_name:'name',su_email:'email',su_pw:'new-password',ph_num:'tel',ph_code:'one-time-code'};
  function hintFields(){
    Object.keys(HINT).forEach(function(id){
      var el=document.getElementById(id); if(!el||el.getAttribute('data-hinted'))return;
      el.setAttribute('autocomplete',HINT[id]); el.setAttribute('data-hinted','1');
      if(id==='li_email'||id==='su_email'){el.setAttribute('name','email');el.setAttribute('inputmode','email');el.setAttribute('autocapitalize','off');el.setAttribute('spellcheck','false');}
      if(id==='li_pw'||id==='su_pw'){el.setAttribute('name','password');}
    });
  }
  new MutationObserver(hintFields).observe(document.documentElement,{childList:true,subtree:true});
  document.addEventListener('DOMContentLoaded',hintFields);
  window.vyToggleSheet=function(){var s=document.querySelector('.sheet');if(s)s.classList.toggle('open');};

  /* ---------------- install as an app (PWA) ---------------- */
  var deferredPrompt = null;
  var isIOS = /iphone|ipad|ipod/i.test(navigator.userAgent) || (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
  var isStandalone = function(){
    return (window.matchMedia && window.matchMedia('(display-mode: standalone)').matches) || navigator.standalone === true;
  };
  function showInstall(){ if(isStandalone())return;
    Array.prototype.forEach.call(document.querySelectorAll('.pwa-install'), function(el){ el.style.display = ''; }); }
  function hideInstall(){
    Array.prototype.forEach.call(document.querySelectorAll('.pwa-install'), function(el){ el.style.display = 'none'; }); }
  function toast(msg){
    var t = document.createElement('div'); t.className = 'vy-toast'; t.textContent = msg;
    document.body.appendChild(t);
    setTimeout(function(){ t.classList.add('go'); }, 30);
    setTimeout(function(){ t.classList.remove('go'); setTimeout(function(){ t.remove(); }, 250); }, 6500);
  }
  window.addEventListener('beforeinstallprompt', function(e){ e.preventDefault(); deferredPrompt = e; showInstall(); });
  window.addEventListener('appinstalled', function(){ deferredPrompt = null; hideInstall(); toast('Rinse is installed. Open it from your desktop or home screen.'); });
  window.vyInstall = function(){
    if (deferredPrompt) {
      deferredPrompt.prompt();
      if (deferredPrompt.userChoice && deferredPrompt.userChoice.then) {
        deferredPrompt.userChoice.then(function(){ deferredPrompt = null; hideInstall(); });
      }
      return;
    }
    if (isIOS) { toast("To install on iPhone or iPad: tap Share, then 'Add to Home Screen'."); return; }
    toast("Look for the install icon in your browser's address bar, or open the browser menu and choose 'Install Rinse'.");
  };
  // iOS Safari never fires beforeinstallprompt, so offer the instructions there.
  if (isIOS && !isStandalone()) { showInstall(); }
  if ('serviceWorker' in navigator) {
    window.addEventListener('load', function(){
      navigator.serviceWorker.register('sw.js', { scope: './' }).catch(function(){});
    });
  }
})();
