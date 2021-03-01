clsWhenLight="dark-mode-disabled";clsWhenDark="dark-mode-enabled";btnId="dark-mode-toggle";faIcon="fa-moon";storeKey="dark-mode";systemPrefersMatch="(prefers-color-scheme: dark)"
function updateIcon(darkMode){btn=document.getElementById(btnId);if(darkMode){btn.classList.remove(clsWhenLight);btn.classList.add(clsWhenDark);}else{btn.classList.remove(clsWhenDark);btn.classList.add(clsWhenLight);}}
function toggleDarkMode(){attr=document.documentElement.getAttribute("data-theme");darkMode=attr==="dark";if(darkMode){setLightMode();updateIcon(false);setUserPreference(false);}else{setDarkMode();updateIcon(true);setUserPreference(true);}}
function setUserPreference(darkMode){pref=darkMode?"true":"false";sessionStorage.setItem(storeKey,pref)}
function clearUserPreference(){sessionStorage.removeItem(storeKey)}
function userPrefersDarkMode(){darkMode=sessionStorage.getItem(storeKey);return darkMode==="true";}
function userPrefersLightMode(){darkMode=sessionStorage.getItem(storeKey);return darkMode==="false";}
function setDarkMode(){document.documentElement.setAttribute("data-theme","dark");}
function setLightMode(){document.documentElement.removeAttribute("data-theme");}
function shouldDarkMode(){if(userPrefersDarkMode()){return true;}else if(userPrefersLightMode()){return false;}else if(window.matchMedia(systemPrefersMatch).matches){return true;}else{return false;}}
if(shouldDarkMode()){setDarkMode();}else{setLightMode();}
window.onload=function(){if(shouldDarkMode()){updateIcon(true);}else{updateIcon(false);}
document.body.classList.remove("preload")
window.matchMedia(systemPrefersMatch).addEventListener("change",event=>{if(event.matches){clearUserPreference();setDarkMode();updateIcon(true);}else{clearUserPreference();setLightMode();updateIcon(false);}});}