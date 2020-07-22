clsWhenLight="dark-mode-disabled";clsWhenDark="dark-mode-enabled";btnId="dark-mode-toggle";storeKey="dark-mode";function isDarkMode(){darkMode=sessionStorage.getItem(storeKey);return darkMode==="true";}
function setDarkMode(darkMode){if(darkMode){document.documentElement.setAttribute("data-theme","dark");sessionStorage.setItem(storeKey,"true");}else{document.documentElement.removeAttribute("data-theme");sessionStorage.removeItem(storeKey);}}
function setDarkModeIcon(darkMode){btn=document.getElementById(btnId);if(darkMode){btn.classList.remove(clsWhenLight);btn.classList.add(clsWhenDark);}else{btn.classList.remove(clsWhenDark);btn.classList.add(clsWhenLight);}}
function toggleDarkMode(){darkMode=isDarkMode();setDarkMode(!darkMode);setDarkModeIcon(!darkMode);}
if(isDarkMode()){document.documentElement.setAttribute("data-theme","dark");}
window.onload=function(){setDarkModeIcon(isDarkMode());}