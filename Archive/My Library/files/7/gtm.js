
// Copyright 2012 Google Inc. All rights reserved.
(function(w,g){w[g]=w[g]||{};w[g].e=function(s){return eval(s);};})(window,'google_tag_manager');(function(){

var data = {
"resource": {
  "version":"206",
  
  "macros":[{
      "function":"__u",
      "vtp_component":"HOST",
      "vtp_enableMultiQueryKeys":false,
      "vtp_enableIgnoreEmptyQueryParam":false
    },{
      "function":"__e"
    },{
      "function":"__smm",
      "vtp_input":["macro",0],
      "vtp_map":["list",["map","key","origin-www2.epa.gov","value","UA-32633028-4"],["map","key","webcms.appdev.epa.gov","value","UA-32633028-3"],["map","key","espanol.appdev.epa.gov","value","UA-32633028-3"],["map","key","wcms.epa.gov","value","UA-32633028-4"],["map","key","es-wcms.epa.gov","value","UA-32633028-4"]],
      "vtp_defaultValue":"UA-32633028-8",
      "vtp_setDefaultValue":true
    },{
      "function":"__v",
      "vtp_name":"gtm.historyChangeSource",
      "vtp_dataLayerVersion":1
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=",["escape",["macro",0],8,16],";\/airnow\\.gov\/.test(a)||\/enviroflash\\.info\/.test(a)?a=\"airnow.gov\":\/energystar\\.gov\/.test(a)?a=\"energystar.gov\":\/fedcenter\\.gov\/.test(a)?a=\"fedcenter.gov\":\/frtr\\.gov\/.test(a)?a=\"frtr.gov\":\/glnpo\\.net\/.test(a)?a=\"glnpo.net\":\/glri\\.us\/.test(a)?a=\"glri.us\":\/regulations\\.gov\/.test(a)?a=\"regulations.gov\":\/sustainability\\.gov\/.test(a)?a=\"sustainability.gov\":\/zendesk\\.com\/.test(a)\u0026\u0026(a=\"zendesk.com\");return a})();"]
    },{
      "function":"__dbg"
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),c,a=0;a\u003Cb.length;a++)if(\"WebArea\"===b[a].getAttribute(\"name\")||\"WebArea\"===b[a].getAttribute(\"property\"))c=b[a].getAttribute(\"content\");return void 0==c?\"No Web Area Defined\":c})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=document.referrer;\/epa(-(otis|echo))?\\.gov\/.test(a)\u0026\u0026(a=\"\");return a})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){return-1\u003Cwindow.location.hostname.indexOf(\"airnow.gov\")?window.location.pathname+window.location.search+window.location.hash:window.location.pathname+window.location.search})();"]
    },{
      "function":"__c",
      "vtp_value":"EPA"
    },{
      "function":"__cid"
    },{
      "function":"__c",
      "vtp_value":"160101"
    },{
      "function":"__ctv"
    },{
      "function":"__c",
      "vtp_value":["template","EPA 3.0 ",["macro",11]," - GTM version ",["macro",12]]
    },{
      "function":"__k",
      "vtp_name":"_ga",
      "vtp_decodeCookie":true
    },{
      "function":"__u",
      "vtp_component":"QUERY",
      "vtp_queryKey":"_ga",
      "vtp_enableMultiQueryKeys":false,
      "vtp_enableIgnoreEmptyQueryParam":false
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=",["escape",["macro",14],8,16],",d=",["escape",["macro",15],8,16],",b=\"one and done visitor\",c=\/_ga=([^\u0026]*)\u0026?\/;c=document.location.hash.match(c);void 0!==a\u0026\u00260\u003Ca.length?(a=a.split(\".\"),b=a[2]):void 0!==d\u0026\u00260\u003Cd.length?(a=d.split(\".\"),b=a[2]):1\u003Cc\u0026\u0026(b=c[1]);return b})();"]
    },{
      "function":"__d",
      "vtp_attributeName":"class",
      "vtp_selectorType":"CSS",
      "vtp_elementSelector":"body"
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=\"not-assigned\",b=",["escape",["macro",17],8,16],";-1!==b.indexOf(\"node-type-web-area\")\u0026\u0026(-1!==b.indexOf(\"microsite\")?a=\"microsite\":-1!==b.indexOf(\"resource-directory\")\u0026\u0026(a=\"resource-directory\"));return a})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){return(document.location.hostname+document.location.pathname).toLowerCase()})();"]
    },{
      "function":"__smm",
      "vtp_input":["macro",18],
      "vtp_setDefaultValue":false,
      "vtp_map":["list",["map","key","microsite","value",["macro",19]],["map","key","not-assigned","value","not-assigned"]]
    },{
      "function":"__smm",
      "vtp_input":["macro",18],
      "vtp_setDefaultValue":false,
      "vtp_map":["list",["map","key","resource-directory","value",["macro",19]],["map","key","not-assigned","value","not-assigned"]]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var b=\"not-assigned\",a=\/.*\\spage-node-([\\d]+)\\s.*\/,c=",["escape",["macro",17],8,16],";(a=c.match(a))\u0026\u00261\u003Ca.length\u0026\u0026(b=a[1]);return b})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var b=\"not-assigned\",a=\/.*\\snode-type-([a-z\\-]+)\\s.*\/,c=",["escape",["macro",17],8,16],";(a=c.match(a))\u0026\u00261\u003Ca.length\u0026\u0026(b=a[1]);return b})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"DC.date.reviewed\"===b[a].getAttribute(\"name\")||\"DC.date.reviewed\"===b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"DC.date.created\"==b[a].getAttribute(\"name\")||\"DC.date.created\"==b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"DC.coverage\"===b[a].getAttribute(\"name\")||\"DC.coverage\"===b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"loginStatus"
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"DC.creator\"===b[a].getAttribute(\"name\")||\"DC.creator\"===b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__u",
      "vtp_component":"PATH",
      "vtp_enableMultiQueryKeys":false,
      "vtp_enableIgnoreEmptyQueryParam":false
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){if(-1\u003C",["escape",["macro",29],8,16],".indexOf(\"\/newsreleases\/\"))return document.getElementsByClassName(\"lineage-item lineage-item-level-0\")[0].innerText})();"]
    },{
      "function":"__smm",
      "vtp_setDefaultValue":true,
      "vtp_input":["macro",4],
      "vtp_defaultValue":"UA-32633028-1",
      "vtp_map":["list",["map","key","origin-www2.epa.gov","value","UA-32633028-4"],["map","key","webcms.appdev.epa.gov","value","UA-32633028-3"],["map","key","espanol.appdev.epa.gov","value","UA-32633028-3"],["map","key","wcms.epa.gov","value","UA-32633028-4"],["map","key","es-wcms.epa.gov","value","UA-32633028-4"],["map","key","airnow.gov","value","UA-32633028-13"],["map","key","energystar.gov","value","UA-32633028-14"],["map","key","zendesk.com","value","UA-32633028-15"],["map","key","regulations.gov","value","UA-32633028-16"],["map","key","fedcenter.gov","value","UA-32633028-17"],["map","key","frtr.gov","value","UA-32633028-18"],["map","key","sustainability.gov","value","UA-32633028-19"],["map","key","glnpo.net","value","UA-32633028-20"],["map","key","glri.us","value","UA-32633028-22"]]
    },{
      "function":"__c",
      "vtp_value":"GSA"
    },{
      "function":"__c",
      "vtp_value":"UA-33523145-1"
    },{
      "function":"__aev",
      "vtp_setDefaultValue":false,
      "vtp_component":"QUERY",
      "vtp_varType":"URL"
    },{
      "function":"__v",
      "vtp_name":"gtm.element",
      "vtp_dataLayerVersion":1
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=",["escape",["macro",34],8,16],",b=\/p_download_id\/,c=\/\\.(pdf|ai|csv|dmg|docx?|eps|exe|gif|ico|jpe?g|json|kml|mov|mp[34]|msi|png|pptx?|psd|rar|smi|swf|tif|txt|xls[xm]?|xml|xsd|zip|ppsx)$\/;if(b.test(a))return a=",["escape",["macro",35],8,16],".innerText.split(\".\"),(1\u003Ca.length?a.pop():\"html\").toLowerCase();if(c.test(a))return a=a.split(\".\"),(1\u003Ca.length?a.pop():\"html\").toLowerCase();a=",["escape",["macro",35],8,16],".pathname.split(\".\");return(1\u003Ca.length?a.pop():\"html\").toLowerCase()})();"]
    },{
      "function":"__v",
      "vtp_name":"gtm.triggers",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":true,
      "vtp_defaultValue":""
    },{
      "function":"__smm",
      "vtp_setDefaultValue":true,
      "vtp_input":["macro",36],
      "vtp_defaultValue":"html",
      "vtp_map":["list",["map","key","ai","value","ai"],["map","key","csv","value","csv"],["map","key","dmg","value","dmg"],["map","key","doc","value","doc"],["map","key","docx","value","doc"],["map","key","eps","value","eps"],["map","key","exe","value","exe"],["map","key","gif","value","gif"],["map","key","ico","value","ico"],["map","key","jpeg","value","jpg"],["map","key","jpg","value","jpg"],["map","key","json","value","json"],["map","key","kml","value","kml"],["map","key","mp3","value","mp3"],["map","key","mp4","value","mp4"],["map","key","msi","value","msi"],["map","key","pdf","value","pdf"],["map","key","png","value","png"],["map","key","ppt","value","ppt"],["map","key","pptx","value","ppt"],["map","key","psd","value","psd"],["map","key","rar","value","rar"],["map","key","smi","value","smi"],["map","key","swf","value","swf"],["map","key","tif","value","tif"],["map","key","txt","value","txt"],["map","key","xls","value","xls"],["map","key","xlsx","value","xls"],["map","key","xlsm","value","xls"],["map","key","xml","value","xml"],["map","key","xsd","value","xsd"],["map","key","zip","value","zip"],["map","key","ppsx","value","pps"],["map","key","7z","value","7z"],["map","key","mov","value","mov"]]
    },{
      "function":"__v",
      "vtp_name":"gtm.elementUrl",
      "vtp_dataLayerVersion":1
    },{
      "function":"__aev",
      "vtp_setDefaultValue":false,
      "vtp_component":"PROTOCOL",
      "vtp_varType":"URL"
    },{
      "function":"__aev",
      "vtp_defaultPages":["list"],
      "vtp_setDefaultValue":false,
      "vtp_component":"PATH",
      "vtp_varType":"URL"
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=\"\",b=",["escape",["macro",41],8,16],";0\u003Cb.length\u0026\u0026(a=b.match(\/\\\/?(.*)\/),a=a[1]);return a})();"]
    },{
      "function":"__v",
      "vtp_name":"gtm.elementClasses",
      "vtp_dataLayerVersion":1
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=\"\",c=",["escape",["macro",43],8,16],",b=",["escape",["macro",35],8,16],";-1\u003Cc.indexOf(\"share-links\")\u0026\u0026(a=\"share-network-unknown\",6\u003Cb.parentNode.className.length\u0026\u0026(a=b.parentNode.getAttribute(\"class\"),a=a.substr(18)));return a})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=\"external\",d=\/(epa(-(otis|echo))?|energystar|airnow|urbanwaters|relocatefeds|lab21century)\\.gov|supportportal\\.com|enviroflash\\.info|zendesk\\.com\/,c=",["escape",["macro",35],8,16],".hostname,b=",["escape",["macro",0],8,16],".split(\".\").slice(-2);b=b.join(\".\").toLowerCase();1\u003E",["escape",["macro",39],8,16],".length?a=\"\":-1\u003C",["escape",["macro",39],8,16],".indexOf(\"javascript\")?a=\"\":d.test(c)\u0026\u0026(a=-1==c.indexOf(b)?\"crossDomain\":\"\");return a})();"]
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"searchPath"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"vpTitle"
    },{
      "function":"__u",
      "vtp_component":"URL",
      "vtp_enableMultiQueryKeys":false,
      "vtp_enableIgnoreEmptyQueryParam":false
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"toPage"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"pagePath"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"pageTitle"
    },{
      "function":"__gas",
      "vtp_cookieDomain":"auto",
      "vtp_doubleClick":false,
      "vtp_setTrackerName":false,
      "vtp_useDebugVersion":false,
      "vtp_fieldsToSet":["list",["map","fieldName","page","value",["macro",50]],["map","fieldName","title","value",["macro",51]]],
      "vtp_useHashAutoLink":false,
      "vtp_decorateFormsAutoLink":false,
      "vtp_enableLinkId":false,
      "vtp_enableEcommerce":false,
      "vtp_trackingId":"UA-32633028-14",
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false
    },{
      "function":"__aev",
      "vtp_varType":"TEXT"
    },{
      "function":"__v",
      "vtp_name":"gtm.elementId",
      "vtp_dataLayerVersion":1
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=document.getElementById(\"address\").value;return a})();"]
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"action"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"label"
    },{
      "function":"__v",
      "vtp_name":"gtm.videoStatus",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.videoPercent",
      "vtp_dataLayerVersion":1
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=",["escape",["macro",58],8,16],";switch(a){case \"start\":return\"started-video\";case \"pause\":return\"paused-video\";case \"buffering\":return\"video-buffering\";case \"progress\":return\"reached-\"+",["escape",["macro",59],8,16],"+\"%-video\";case \"complete\":return\"completed-video\";default:return ",["escape",["macro",58],8,16],"}})();"]
    },{
      "function":"__v",
      "vtp_name":"gtm.videoTitle",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.videoUrl",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.elementId",
      "vtp_dataLayerVersion":1
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=document.getElementById(\"search\").value;return a})();"]
    },{
      "function":"__smm",
      "vtp_input":["macro",0],
      "vtp_map":["list",["map","key","origin-www2.epa.gov","value","UA-32633028-4"],["map","key","webcms.appdev.epa.gov","value","UA-32633028-3"],["map","key","espanol.appdev.epa.gov","value","UA-32633028-3"],["map","key","wcms.epa.gov","value","UA-32633028-4"],["map","key","es-wcms.epa.gov","value","UA-32633028-4"]],
      "vtp_defaultValue":"UA-32633028-1",
      "vtp_setDefaultValue":true
    },{
      "function":"__smm",
      "vtp_input":["macro",0],
      "vtp_map":["list",["map","key","webcms.appdev.epa.gov","value","false"],["map","key","espanol.appdev.epa.gov","value","false"],["map","key","wcms.epa.gov","value","false"],["map","key","es-wcms.epa.gov","value","false"]],
      "vtp_defaultValue":"true",
      "vtp_setDefaultValue":true
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=!0;\"false\"==",["escape",["macro",66],8,16],"\u0026\u0026(a=!1);return a})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=!1,b=",["escape",["macro",15],8,16],";void 0!==b\u0026\u00260\u003Cb.length\u0026\u0026(a=!0);return a})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"DC.description\"===b[a].getAttribute(\"name\")||\"DC.description\"===b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){var a=",["escape",["macro",69],8,16],";a=a.toLowerCase();if(-1!==a.indexOf(\"covid\")||-1!==a.indexOf(\"coronavirus\"))return!0})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){return document.title})();"]
    },{
      "function":"__c",
      "vtp_value":"UA-32633028-3"
    },{
      "function":"__c",
      "vtp_value":"epa.gov, epa-otis.gov, epa-echo.gov, energystar.gov, enviroflash.info, airnow.gov, urbanwaters.gov, relocatefeds.gov, lab21century.gov, supportportal.com, zendesk.com"
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){return function(){if(-1\u003Cdocument.location.hash.indexOf(\"_ga\\x3d\")){var b=document.location.hash,a=window.location;document.location.hash=b.replace(\/_ga=[^\u0026]*\u0026?\/,\"\");\"\\x26\"===document.location.hash.split(\"\").pop()\u0026\u0026(document.location.hash=document.location.hash.slice(0,-1));\"#\"===document.location.href.split(\"\").pop()\u0026\u0026history\u0026\u0026history.replaceState\u0026\u0026history.replaceState(\"\",document.title,a.pathname+a.search)}}})();"]
    },{
      "function":"__aev",
      "vtp_stripWww":true,
      "vtp_setDefaultValue":false,
      "vtp_component":"HOST",
      "vtp_varType":"URL"
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"WebAreaType\"===b[a].getAttribute(\"name\")||\"WebAreaType\"===b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__jsm",
      "vtp_javascript":["template","(function(){for(var b=document.getElementsByTagName(\"meta\"),a=0;a\u003Cb.length;a++)if(\"ContentType\"===b[a].getAttribute(\"name\")||\"ContentType\"===b[a].getAttribute(\"property\"))return b[a].getAttribute(\"content\")})();"]
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"fromPage"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"productType"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"eventCat"
    },{
      "function":"__v",
      "vtp_dataLayerVersion":2,
      "vtp_setDefaultValue":false,
      "vtp_name":"eventAct"
    },{
      "function":"__f",
      "vtp_component":"URL"
    },{
      "function":"__e"
    },{
      "function":"__v",
      "vtp_name":"gtm.elementTarget",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.element",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.elementClasses",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.newUrlFragment",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.videoProvider",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.videoDuration",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.videoVisible",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.videoCurrentTime",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.visibleRatio",
      "vtp_dataLayerVersion":1
    },{
      "function":"__v",
      "vtp_name":"gtm.visibleTime",
      "vtp_dataLayerVersion":1
    }],
  "tags":[{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_useDebugVersion":["macro",5],
      "vtp_useHashAutoLink":false,
      "vtp_trackType":"TRACK_PAGEVIEW",
      "vtp_contentGroup":["list",["map","index","1","group",["macro",6]]],
      "vtp_decorateFormsAutoLink":false,
      "vtp_overrideGaSettings":true,
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]],["map","fieldName","page","value",["macro",8]]],
      "vtp_trackerName":["macro",9],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","19","dimension",["macro",10]],["map","index","18","dimension",["macro",13]],["map","index","1","dimension",["macro",16]],["map","index","2","dimension",["macro",20]],["map","index","3","dimension",["macro",21]],["map","index","4","dimension",["macro",22]],["map","index","20","dimension",["macro",23]],["map","index","32","dimension",["macro",24]],["map","index","33","dimension",["macro",25]],["map","index","34","dimension",["macro",26]],["map","index","35","dimension",["macro",27]],["map","index","36","dimension",["macro",28]],["map","index","37","dimension",["macro",30]]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "tag_id":18
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_useDebugVersion":["macro",5],
      "vtp_useHashAutoLink":false,
      "vtp_trackType":"TRACK_PAGEVIEW",
      "vtp_decorateFormsAutoLink":false,
      "vtp_overrideGaSettings":true,
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",32],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","4","dimension",["macro",10]],["map","index","3","dimension",["macro",13]],["map","index","2","dimension",["template","EPA - ",["macro",0]]],["map","index","1","dimension","EPA"]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",33],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "tag_id":19
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":"Download",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":["template",["macro",38]," Click"],
      "vtp_eventLabel":["macro",39],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",9],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","19","dimension",["macro",10]],["map","index","18","dimension",["macro",13]],["map","index","1","dimension",["macro",16]],["map","index","2","dimension",["macro",20]],["map","index","3","dimension",["macro",21]],["map","index","4","dimension",["macro",22]],["map","index","20","dimension",["macro",23]]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":21
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":"Download",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":["template",["macro",38]," Click"],
      "vtp_eventLabel":["macro",39],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",32],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","4","dimension",["macro",10]],["map","index","3","dimension",["macro",13]],["map","index","2","dimension",["template","EPA - ",["macro",0]]],["map","index","1","dimension","EPA"]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",33],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":22
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":"Email",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Link Click",
      "vtp_eventLabel":["macro",42],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",9],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","19","dimension",["macro",10]],["map","index","18","dimension",["macro",13]],["map","index","1","dimension",["macro",16]],["map","index","2","dimension",["macro",20]],["map","index","3","dimension",["macro",21]],["map","index","4","dimension",["macro",22]],["map","index","20","dimension",["macro",23]]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":23
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":"Email",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Link click",
      "vtp_eventLabel":["macro",42],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",32],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","4","dimension",["macro",10]],["map","index","3","dimension",["macro",13]],["map","index","2","dimension",["template","EPA - ",["macro",0]]],["map","index","1","dimension","EPA"]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",33],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":24
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":"Share",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":["macro",44],
      "vtp_eventLabel":["macro",39],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",9],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","19","dimension",["macro",10]],["map","index","18","dimension",["macro",13]],["map","index","1","dimension",["macro",16]],["map","index","2","dimension",["macro",20]],["map","index","3","dimension",["macro",21]],["map","index","4","dimension",["macro",22]],["map","index","20","dimension",["macro",23]]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":25
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":["macro",45],
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Link Click",
      "vtp_eventLabel":["macro",39],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",9],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","19","dimension",["macro",10]],["map","index","18","dimension",["macro",13]],["map","index","1","dimension",["macro",16]],["map","index","2","dimension",["macro",20]],["map","index","3","dimension",["macro",21]],["map","index","4","dimension",["macro",22]],["map","index","20","dimension",["macro",23]]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":27
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_eventCategory":["macro",45],
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Link Click",
      "vtp_eventLabel":["macro",39],
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",32],
      "vtp_enableLinkId":false,
      "vtp_dimension":["list",["map","index","4","dimension",["macro",10]],["map","index","3","dimension",["macro",13]],["map","index","2","dimension",["template","EPA - ",["macro",0]]],["map","index","1","dimension","EPA"]],
      "vtp_enableEcommerce":false,
      "vtp_trackingId":["macro",33],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":28
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_setTrackerName":true,
      "vtp_doubleClick":false,
      "vtp_useDebugVersion":["macro",5],
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","referrer","value",["macro",7]]],
      "vtp_trackerName":["macro",9],
      "vtp_trackType":"TRACK_SOCIAL",
      "vtp_socialAction":"share click",
      "vtp_enableLinkId":false,
      "vtp_socialActionTarget":["macro",39],
      "vtp_socialNetwork":["macro",44],
      "vtp_dimension":["list",["map","index","19","dimension",["macro",10]],["map","index","18","dimension",["macro",13]],["map","index","1","dimension",["macro",16]],["map","index","2","dimension",["macro",20]],["map","index","3","dimension",["macro",21]],["map","index","4","dimension",["macro",22]],["map","index","20","dimension",["macro",23]]],
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsSocial":true,
      "tag_id":29
    },{
      "function":"__ua",
      "once_per_event":true,
      "vtp_overrideGaSettings":true,
      "vtp_fieldsToSet":["list",["map","fieldName","cookieDomain","value","auto"],["map","fieldName","page","value",["template","\/virtual",["macro",46]]],["map","fieldName","title","value",["macro",47]]],
      "vtp_trackType":"TRACK_PAGEVIEW",
      "vtp_trackingId":"UA-32633028-1",
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "tag_id":36
    },{
      "function":"__paused",
      "vtp_originalTagType":"ua",
      "tag_id":38
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"crwu-rsg-navigate",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":["macro",49],
      "vtp_trackingId":"UA-32633028-1",
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":42
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_overrideGaSettings":false,
      "vtp_trackType":"TRACK_PAGEVIEW",
      "vtp_gaSettings":["macro",52],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "tag_id":44
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"crwu-rsg-navigate",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"0-info-pages",
      "vtp_eventLabel":["macro",53],
      "vtp_trackingId":"UA-32633028-1",
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":45
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"crwu-rsg-navigate",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"7-generate-report",
      "vtp_trackingId":"UA-32633028-1",
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":46
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"crwu-rsg-navigate",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"8-generate-CREAT-export",
      "vtp_trackingId":"UA-32633028-1",
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":47
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"utility-search",
      "vtp_eventLabel":["macro",55],
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":54
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"homes-application-click",
      "vtp_eventLabel":"builders-start-app",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":55
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"homes-application-click",
      "vtp_eventLabel":"raters-start-app",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":56
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"homes-application-click",
      "vtp_eventLabel":"utilities-start-app",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":57
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"choose-a-light-start",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":58
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-start",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":64
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"choose-a-light-finish",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":71
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":["macro",56],
      "vtp_eventLabel":["macro",57],
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":72
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":["macro",60],
      "vtp_eventLabel":["template",["macro",61]," - ",["macro",62]],
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":73
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"subscribe",
      "vtp_eventLabel":"buildings-mailing",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":74
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Sign-in",
      "vtp_eventLabel":"mesa-login",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":75
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Sign-in",
      "vtp_eventLabel":"portfolio-manager-login",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":76
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"Sign-in",
      "vtp_eventLabel":"myES-forTheHome-login",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":77
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"create-account",
      "vtp_eventLabel":"myES-forTheHome-newAccount",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":78
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"lp-search",
      "vtp_eventLabel":["macro",64],
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":79
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step1-no",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":80
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step1-yes",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":81
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step2-no",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":82
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step2-yes",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":83
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step3-no",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":84
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step3-yes",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":85
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step4-no",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":86
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-step4-yes",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":87
    },{
      "function":"__ua",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_nonInteraction":false,
      "vtp_overrideGaSettings":true,
      "vtp_eventCategory":"Energy Star",
      "vtp_trackType":"TRACK_EVENT",
      "vtp_eventAction":"button-click",
      "vtp_eventLabel":"wh-replacement-guide-to-wh-productfinder-button",
      "vtp_trackingId":["macro",31],
      "vtp_enableRecaptchaOption":false,
      "vtp_enableTransportUrl":false,
      "vtp_enableUaRlsa":false,
      "vtp_enableUseInternalVersion":false,
      "vtp_enableFirebaseCampaignData":true,
      "vtp_trackTypeIsEvent":true,
      "tag_id":93
    },{
      "function":"__lcl",
      "vtp_waitForTags":false,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"200",
      "vtp_uniqueTriggerId":"42870_51",
      "tag_id":205
    },{
      "function":"__lcl",
      "vtp_waitForTags":true,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"200",
      "vtp_uniqueTriggerId":"42870_52",
      "tag_id":206
    },{
      "function":"__lcl",
      "vtp_waitForTags":true,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"200",
      "vtp_uniqueTriggerId":"42870_53",
      "tag_id":207
    },{
      "function":"__lcl",
      "vtp_waitForTags":true,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"200",
      "vtp_uniqueTriggerId":"42870_54",
      "tag_id":208
    },{
      "function":"__lcl",
      "vtp_waitForTags":true,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"200",
      "vtp_uniqueTriggerId":"42870_55",
      "tag_id":209
    },{
      "function":"__hl",
      "tag_id":210
    },{
      "function":"__evl",
      "vtp_elementId":"search-button",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":true,
      "vtp_firingFrequency":"ONCE",
      "vtp_selectorType":"ID",
      "vtp_onScreenRatio":"50",
      "vtp_uniqueTriggerId":"42870_124",
      "tag_id":211
    },{
      "function":"__cl",
      "tag_id":212
    },{
      "function":"__lcl",
      "vtp_waitForTags":false,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_158",
      "tag_id":213
    },{
      "function":"__lcl",
      "vtp_waitForTags":false,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_159",
      "tag_id":214
    },{
      "function":"__lcl",
      "vtp_waitForTags":false,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_160",
      "tag_id":215
    },{
      "function":"__lcl",
      "vtp_waitForTags":false,
      "vtp_checkValidation":false,
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_161",
      "tag_id":216
    },{
      "function":"__cl",
      "tag_id":217
    },{
      "function":"__cl",
      "tag_id":218
    },{
      "function":"__cl",
      "tag_id":219
    },{
      "function":"__ytl",
      "vtp_progressThresholdsPercent":"25, 50, 75",
      "vtp_captureComplete":true,
      "vtp_captureStart":true,
      "vtp_fixMissingApi":true,
      "vtp_triggerStartOption":"WINDOW_LOAD",
      "vtp_radioButtonGroup1":"PERCENTAGE",
      "vtp_capturePause":true,
      "vtp_captureProgress":true,
      "vtp_uniqueTriggerId":"42870_175",
      "vtp_enableTriggerStartOption":true,
      "tag_id":220
    },{
      "function":"__fsl",
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_177",
      "tag_id":221
    },{
      "function":"__fsl",
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_178",
      "tag_id":222
    },{
      "function":"__fsl",
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_179",
      "tag_id":223
    },{
      "function":"__fsl",
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_180",
      "tag_id":224
    },{
      "function":"__fsl",
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_181",
      "tag_id":225
    },{
      "function":"__fsl",
      "vtp_waitForTagsTimeout":"2000",
      "vtp_uniqueTriggerId":"42870_182",
      "tag_id":226
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.check.quiz-electric-yes",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_187",
      "tag_id":227
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.minus.quiz-electric-no",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_188",
      "tag_id":228
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.minus.quiz-air-no",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_189",
      "tag_id":229
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.check.quiz-air-yes",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_190",
      "tag_id":230
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.check.quiz-head-yes",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_191",
      "tag_id":231
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.minus.quiz-head-no",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_192",
      "tag_id":232
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.check.quiz-water-yes",
      "vtp_firingFrequency":"ONCE_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_193",
      "tag_id":233
    },{
      "function":"__evl",
      "vtp_useOnScreenDuration":false,
      "vtp_useDomChangeListener":false,
      "vtp_elementSelector":"li.minus.quiz-water-no",
      "vtp_firingFrequency":"MANY_PER_ELEMENT",
      "vtp_selectorType":"CSS",
      "vtp_onScreenRatio":"10",
      "vtp_uniqueTriggerId":"42870_194",
      "tag_id":234
    },{
      "function":"__cl",
      "tag_id":235
    },{
      "function":"__html",
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/javascript\"\u003E(function(b){var a=document,e=a.createElement(\"script\");a=a.head||a.getElementsByTagName(\"head\")[0];var c=\"fsReady\",f={src:\"\/\/gateway.foresee.com\/sites\/epa-gov\/staging\/gateway.min.js\",type:\"text\/javascript\",async:\"true\",\"data-vendor\":\"fs\",\"data-role\":\"gateway\"},d;for(d in f)e.setAttribute(d,f[d]);a.appendChild(e);b[c]||(b[c]=function(){var a=\"__\"+c+\"_stk__\";b[a]=b[a]||[];b[a].push(arguments)})})(window);\u003C\/script\u003E",
      "vtp_supportDocumentWrite":true,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "vtp_usePostscribe":true,
      "tag_id":9
    },{
      "function":"__html",
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/javascript\"\u003E(function(b){var a=document,e=a.createElement(\"script\");a=a.head||a.getElementsByTagName(\"head\")[0];var c=\"fsReady\",f={src:\"\/\/gateway.foresee.com\/sites\/epa-gov\/production\/gateway.min.js\",type:\"text\/javascript\",async:\"true\",\"data-vendor\":\"fs\",\"data-role\":\"gateway\"},d;for(d in f)e.setAttribute(d,f[d]);a.appendChild(e);b[c]||(b[c]=function(){var a=\"__\"+c+\"_stk__\";b[a]=b[a]||[];b[a].push(arguments)})})(window);\u003C\/script\u003E",
      "vtp_supportDocumentWrite":true,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "vtp_usePostscribe":true,
      "tag_id":10
    },{
      "function":"__html",
      "live_only":true,
      "metadata":["map"],
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/gtmscript\" data-gtmsrc=\"\/\/script.crazyegg.com\/pages\/scripts\/0005\/9240.js\" async=\"async\"\u003E\u003C\/script\u003E",
      "vtp_supportDocumentWrite":false,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "tag_id":32
    },{
      "function":"__html",
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/gtmscript\"\u003E(function(){jQuery(\".input-group .form-control.ng-valid.ng-not-empty.ng-dirty.ng-valid-parse.ng-touched\");var c=jQuery(\".form-control:first\");jQuery(\"#search-button\");jQuery(c).on(\"keydown\",function(a){var b=c.val();a=a.which;b=window.location.pathname+\"?querytext\\x3d\"+b;13==a\u0026\u0026(dataLayer.push({event:\"VPTracker\",searchPath:b,vpTitle:\"Basic EPA Fusion Search Results\"}),function(){var a=1E3;window.setTimeout(function(){window.dataLayer.push({event:\"1_second_timer_hashchange_onPressEnter\"})},a)}())})})();\u003C\/script\u003E",
      "vtp_supportDocumentWrite":false,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "tag_id":37
    },{
      "function":"__html",
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/gtmscript\"\u003E(function(){var a=1500;window.setTimeout(function(){window.dataLayer.push({event:\"1_second_timer_hashchange\"})},a)})();\u003C\/script\u003E",
      "vtp_supportDocumentWrite":false,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "tag_id":39
    },{
      "function":"__html",
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/gtmscript\"\u003E(function(){jQuery(\".input-group .form-control.ng-valid.ng-not-empty.ng-dirty.ng-valid-parse.ng-touched\");jQuery(\".form-control:first\");jQuery(\"#search-button\");jQuery(\"button.epa-search-buttonx\").on(\"click\",function(a){a=jQuery(\".form-control:first\");a=a.val();a=window.location.pathname+\"?querytext\\x3d\"+a;dataLayer.push({event:\"VPTracker\",searchPath:a,vpTitle:\"Basic EPA Fusion Search Results\"});(function(){var a=1E3;window.setTimeout(function(){window.dataLayer.push({event:\"1_second_timer_hashchange_onPressSearch\"})},\na)})()})})();\u003C\/script\u003E",
      "vtp_supportDocumentWrite":false,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "tag_id":40
    },{
      "function":"__html",
      "once_per_event":true,
      "vtp_html":"\u003Cscript type=\"text\/gtmscript\"\u003E(function(){jQuery(\".input-group .form-control.ng-valid.ng-not-empty.ng-dirty.ng-valid-parse.ng-touched\");jQuery(\".form-control:first\");jQuery(\"#search-button\");jQuery(\"ul.dropdown-menu.ng-isolate-scope\").on(\"click\",function(a){a=jQuery(\".form-control:first\");a=a.val();a=window.location.pathname+\"?querytext\\x3d\"+a;dataLayer.push({event:\"VPTracker\",searchPath:a,vpTitle:\"Basic EPA Fusion Search Results\"});(function(){var a=1E3;window.setTimeout(function(){window.dataLayer.push({event:\"1_second_timer_hashchange_onPressSuggestion\"})},\na)})()})})();\u003C\/script\u003E",
      "vtp_supportDocumentWrite":false,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "tag_id":41
    },{
      "function":"__html",
      "metadata":["map"],
      "once_per_event":true,
      "vtp_html":["template","\u003Cscript type=\"application\/ld+json\"\u003E\n  {\n    \"@context\": \"https:\/\/schema.org\",\n    \"@type\": \"SpecialAnnouncement\",\n    \"name\": \"",["escape",["macro",71],7],"\",\n    \"text\": \"",["escape",["macro",69],7],"\",\n    \"datePosted\": \"",["escape",["macro",25],7],"\",\n    \"expires\": \"",["escape",["macro",24],7],"\",\n    \"category\": \"https:\/\/www.wikidata.org\/wiki\/Q81068910\",\n    \"newsUpdatesAndGuidelines\": \"https:\/\/www.epa.gov\/coronavirus\",\n    \"spatialCoverage\": [\n      {\n        \"type\": \"Country\",\n        \"name\": \"US\"\n      }\n    ]\n  }\n\u003C\/script\u003E"],
      "vtp_supportDocumentWrite":false,
      "vtp_enableIframeMode":false,
      "vtp_enableEditJsMacroBehavior":false,
      "tag_id":204
    }],
  "predicates":[{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"drupal"
    },{
      "function":"_re",
      "arg0":["macro",1],
      "arg1":".*"
    },{
      "function":"_eq",
      "arg0":["macro",3],
      "arg1":"popstate"
    },{
      "function":"_eq",
      "arg0":["macro",4],
      "arg1":"airnow.gov"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.historyChange"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.js"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"eventTracker"
    },{
      "function":"_re",
      "arg0":["macro",0],
      "arg1":".*(dev|prep|stage|test).*",
      "ignore_case":true
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"wcms"
    },{
      "function":"_re",
      "arg0":["macro",36],
      "arg1":"^(ai|csv|dmg|docx?|eps|exe|gif|ico|jpe?g|json|kml|mov|mp[34]|msi|pdf|png|ppsx?|pptx?|psd|rar|smi|swf|tif|txt|xlsx?|xlsm|xml|xsd|zip|7z)$",
      "ignore_case":true
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.linkClick"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_51($|,)))"
    },{
      "function":"_cn",
      "arg0":["macro",34],
      "arg1":"p_download_id"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_55($|,)))"
    },{
      "function":"_sw",
      "arg0":["macro",40],
      "arg1":"mailto"
    },{
      "function":"_re",
      "arg0":["macro",41],
      "arg1":".*[a-z]+.*",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_52($|,)))"
    },{
      "function":"_cn",
      "arg0":["macro",43],
      "arg1":"share"
    },{
      "function":"_re",
      "arg0":["macro",44],
      "arg1":"facebook|twitter|pin(terest)?|g(oogle)?plus|email",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_53($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",45],
      "arg1":"crossDomain|external",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_54($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"VPTracker"
    },{
      "function":"_cn",
      "arg0":["macro",48],
      "arg1":"\/crwu\/"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"rsg-navigate"
    },{
      "function":"_cn",
      "arg0":["macro",48],
      "arg1":"energystar.gov"
    },{
      "function":"_re",
      "arg0":["macro",50],
      "arg1":".+"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"ES - ProductFinderEngine - Application Pageview"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"rsg-infoLinks"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"rsg-exportReport"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"rsg-exportCREAT"
    },{
      "function":"_eq",
      "arg0":["macro",54],
      "arg1":"geocode"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov(\/buildings\/utility_map|\/buildings\/owners_and_managers\/existing_buildings\/use_portfolio_manager\/find_utilities_provide_data_benchmarking).*",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_158($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",39],
      "arg1":"https:\/\/energystar.secure.force.com\/opa\/residential\/builder"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov\/partner_resources\/join_energy_star\/new_home_construction",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_159($|,)))"
    },{
      "function":"_cn",
      "arg0":["macro",39],
      "arg1":"https:\/\/energystar.secure.force.com\/opa\/residential\/verification-organization"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_160($|,)))"
    },{
      "function":"_cn",
      "arg0":["macro",39],
      "arg1":"\/index.cfm?c=join.reps_agree"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_161($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",54],
      "arg1":"edit-next1"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov\/products\/choose_a_light",
      "ignore_case":true
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.click"
    },{
      "function":"_eq",
      "arg0":["macro",54],
      "arg1":"quiz-start"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov\/products\/hot-water-heater-replacement-guide",
      "ignore_case":true
    },{
      "function":"_cn",
      "arg0":["macro",54],
      "arg1":"edit-next4"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"productfinderEngineManualEventTracking"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov.*",
      "ignore_case":true
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.video"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_175($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",63],
      "arg1":"es-bp-email-list-block-form"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.formSubmit"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_177($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",63],
      "arg1":"mesaForm"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_178($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",63],
      "arg1":"pmForm"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_179($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",63],
      "arg1":"homeForm"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_180($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",63],
      "arg1":"createAccountForm"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov\/campaign\/createAccount.*",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_181($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",63],
      "arg1":"lplanding"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov\/buildings\/lp_finder",
      "ignore_case":true
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_182($|,)))"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.elementVisibility"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_188($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_187($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_189($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_190($|,)))"
    },{
      "function":"_cn",
      "arg0":["macro",48],
      "arg1":".*energystar\\.gov\/products\/hot-water-heater-replacement-guide"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_192($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_191($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_194($|,)))"
    },{
      "function":"_re",
      "arg0":["macro",37],
      "arg1":"(^$|((^|,)42870_193($|,)))"
    },{
      "function":"_cn",
      "arg0":["macro",53],
      "arg1":"FIND PRODUCTS, REBATES, AND INSTALLERS"
    },{
      "function":"_re",
      "arg0":["macro",29],
      "arg1":".*",
      "ignore_case":true
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.load"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"drupal3-preprod"
    },{
      "function":"_re",
      "arg0":["macro",48],
      "arg1":".*"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"gtm.dom"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"appdev"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"origin-www2"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"airnow"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"energystar"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"forumone"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"amazonaws"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"regulations"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"ofee"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"fedcenter"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"response"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"lead"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"stashed"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"mywaterway.epa.gov"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"1_second_timer_hashchange"
    },{
      "function":"_cn",
      "arg0":["macro",0],
      "arg1":"search.epa.gov"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"1_second_timer_hashchange_onPressEnter"
    },{
      "function":"_eq",
      "arg0":["macro",1],
      "arg1":"1_second_timer_hashchange_onPressSuggestion"
    },{
      "function":"_sw",
      "arg0":["macro",48],
      "arg1":"\/coronavirus"
    },{
      "function":"_eq",
      "arg0":["macro",70],
      "arg1":"true"
    }],
  "rules":[
    [["if",2,3,4],["add",0]],
    [["if",5],["add",0,1,41,46,47,48,49,50,51,52,53,54,55,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71]],
    [["if",6],["add",0]],
    [["if",9,10,11],["add",2,3],["block",7,8]],
    [["if",10,12,13],["add",2,3],["block",7,8]],
    [["if",10,14,15,16],["add",4,5],["block",7,8]],
    [["if",10,17,18,19],["add",6,9],["block",7,8]],
    [["if",10,20,21],["add",7,8]],
    [["if",22],["add",10,11]],
    [["if",23,24],["add",12]],
    [["if",25,26,27],["add",13]],
    [["if",23,28],["add",14]],
    [["if",23,29],["add",15]],
    [["if",23,30],["add",16]],
    [["if",10,31,32,33],["add",17]],
    [["if",10,34,35,36],["add",18]],
    [["if",10,35,37,38],["add",19]],
    [["if",10,35,39,40],["add",20]],
    [["if",41,42,43],["add",21]],
    [["if",43,44,45],["add",22]],
    [["if",42,43,46],["add",23]],
    [["if",25,47],["add",24]],
    [["if",48,49,50],["add",25]],
    [["if",48,51,52,53],["add",26]],
    [["if",48,52,54,55],["add",27]],
    [["if",48,52,56,57],["add",28]],
    [["if",48,52,58,59],["add",29]],
    [["if",52,60,61,62],["add",30]],
    [["if",52,63,64,65],["add",31]],
    [["if",45,66,67],["add",32]],
    [["if",45,66,68],["add",33]],
    [["if",45,66,69],["add",34]],
    [["if",45,66,70],["add",35]],
    [["if",66,71,72],["add",36]],
    [["if",45,66,73],["add",37]],
    [["if",45,66,74],["add",38]],
    [["if",45,66,75],["add",39]],
    [["if",43,45,76],["add",40]],
    [["if",5,77],["add",42,43,44,45]],
    [["if",78],["add",56]],
    [["if",5,79],["add",72]],
    [["if",80,81],["add",73,74]],
    [["if",95],["add",75,77,78]],
    [["if",81,96],["add",76]],
    [["if",97],["add",77]],
    [["if",98],["add",77]],
    [["if",5,99],["add",79]],
    [["if",5,100],["add",79]],
    [["if",0,1],["block",0,1,73,74]],
    [["if",1,7],["block",0,1,73,74]],
    [["if",1,8],["block",1,73,74]],
    [["if",1,82],["block",73,74]],
    [["if",1,83],["block",73,74]],
    [["if",1,84],["block",73]],
    [["if",1,85],["block",73]],
    [["if",1,86],["block",73,74]],
    [["if",1,87],["block",73,74]],
    [["if",1,88],["block",73]],
    [["if",1,89],["block",73]],
    [["if",1,90],["block",73]],
    [["if",1,91],["block",73]],
    [["if",1,92],["block",73]],
    [["if",1,93],["block",73]],
    [["if",1,94],["block",74]]]
},
"runtime":[]




};
/*

 Copyright The Closure Library Authors.
 SPDX-License-Identifier: Apache-2.0
*/
var aa,ca="function"==typeof Object.create?Object.create:function(a){var b=function(){};b.prototype=a;return new b},da;if("function"==typeof Object.setPrototypeOf)da=Object.setPrototypeOf;else{var fa;a:{var ha={Ff:!0},ia={};try{ia.__proto__=ha;fa=ia.Ff;break a}catch(a){}fa=!1}da=fa?function(a,b){a.__proto__=b;if(a.__proto__!==b)throw new TypeError(a+" is not extensible");return a}:null}
var ja=da,ka=function(a,b){a.prototype=ca(b.prototype);a.prototype.constructor=a;if(ja)ja(a,b);else for(var c in b)if("prototype"!=c)if(Object.defineProperties){var d=Object.getOwnPropertyDescriptor(b,c);d&&Object.defineProperty(a,c,d)}else a[c]=b[c]},ma=this||self,oa=/^[\w+/_-]+[=]{0,2}$/,pa=null,qa=function(a,b){function c(){}c.prototype=b.prototype;a.prototype=new c;a.prototype.constructor=a},ra=function(a){return a};var sa=function(){},ua=function(a){return"function"==typeof a},g=function(a){return"string"==typeof a},va=function(a){return"number"==typeof a&&!isNaN(a)},xa=function(a){return"[object Array]"==Object.prototype.toString.call(Object(a))},ya=function(a,b){if(Array.prototype.indexOf){var c=a.indexOf(b);return"number"==typeof c?c:-1}for(var d=0;d<a.length;d++)if(a[d]===b)return d;return-1},za=function(a,b){if(a&&xa(a))for(var c=0;c<a.length;c++)if(a[c]&&b(a[c]))return a[c]},Aa=function(a,b){if(!va(a)||
!va(b)||a>b)a=0,b=2147483647;return Math.floor(Math.random()*(b-a+1)+a)},Ca=function(a,b){for(var c=new Ba,d=0;d<a.length;d++)c.set(a[d],!0);for(var e=0;e<b.length;e++)if(c.get(b[e]))return!0;return!1},Da=function(a,b){for(var c in a)Object.prototype.hasOwnProperty.call(a,c)&&b(c,a[c])},Ea=function(a){return Math.round(Number(a))||0},Fa=function(a){return"false"==String(a).toLowerCase()?!1:!!a},Ga=function(a){var b=[];if(xa(a))for(var c=0;c<a.length;c++)b.push(String(a[c]));return b},Ha=function(a){return a?
a.replace(/^\s+|\s+$/g,""):""},Ia=function(){return(new Date).getTime()},Ba=function(){this.prefix="gtm.";this.values={}};Ba.prototype.set=function(a,b){this.values[this.prefix+a]=b};Ba.prototype.get=function(a){return this.values[this.prefix+a]};
var Ja=function(a,b,c){return a&&a.hasOwnProperty(b)?a[b]:c},Ka=function(a){var b=!1;return function(){if(!b)try{a()}catch(c){}b=!0}},La=function(a,b){for(var c in b)b.hasOwnProperty(c)&&(a[c]=b[c])},Ma=function(a){for(var b in a)if(a.hasOwnProperty(b))return!0;return!1},Na=function(a,b){for(var c=[],d=0;d<a.length;d++)c.push(a[d]),c.push.apply(c,b[a[d]]||[]);return c},Oa=function(a,b){for(var c={},d=c,e=a.split("."),f=0;f<e.length-1;f++)d=d[e[f]]={};d[e[e.length-1]]=b;return c},Pa=function(a){var b=
[];Da(a,function(c,d){10>c.length&&d&&b.push(c)});return b.join(",")};/*
 jQuery v1.9.1 (c) 2005, 2012 jQuery Foundation, Inc. jquery.org/license. */
var Qa=/\[object (Boolean|Number|String|Function|Array|Date|RegExp)\]/,Ra=function(a){if(null==a)return String(a);var b=Qa.exec(Object.prototype.toString.call(Object(a)));return b?b[1].toLowerCase():"object"},Sa=function(a,b){return Object.prototype.hasOwnProperty.call(Object(a),b)},Ua=function(a){if(!a||"object"!=Ra(a)||a.nodeType||a==a.window)return!1;try{if(a.constructor&&!Sa(a,"constructor")&&!Sa(a.constructor.prototype,"isPrototypeOf"))return!1}catch(c){return!1}for(var b in a);return void 0===
b||Sa(a,b)},p=function(a,b){var c=b||("array"==Ra(a)?[]:{}),d;for(d in a)if(Sa(a,d)){var e=a[d];"array"==Ra(e)?("array"!=Ra(c[d])&&(c[d]=[]),c[d]=p(e,c[d])):Ua(e)?(Ua(c[d])||(c[d]={}),c[d]=p(e,c[d])):c[d]=e}return c};
var Va=[],Wa={"\x00":"&#0;",'"':"&quot;","&":"&amp;","'":"&#39;","<":"&lt;",">":"&gt;","\t":"&#9;","\n":"&#10;","\x0B":"&#11;","\f":"&#12;","\r":"&#13;"," ":"&#32;","-":"&#45;","/":"&#47;","=":"&#61;","`":"&#96;","\u0085":"&#133;","\u00a0":"&#160;","\u2028":"&#8232;","\u2029":"&#8233;"},Xa=function(a){return Wa[a]},Ya=/[\x00\x22\x26\x27\x3c\x3e]/g;var bb=/[\x00\x08-\x0d\x22\x26\x27\/\x3c-\x3e\\\x85\u2028\u2029]/g,cb={"\x00":"\\x00","\b":"\\x08","\t":"\\t","\n":"\\n","\x0B":"\\x0b",
"\f":"\\f","\r":"\\r",'"':"\\x22","&":"\\x26","'":"\\x27","/":"\\/","<":"\\x3c","=":"\\x3d",">":"\\x3e","\\":"\\\\","\u0085":"\\x85","\u2028":"\\u2028","\u2029":"\\u2029",$:"\\x24","(":"\\x28",")":"\\x29","*":"\\x2a","+":"\\x2b",",":"\\x2c","-":"\\x2d",".":"\\x2e",":":"\\x3a","?":"\\x3f","[":"\\x5b","]":"\\x5d","^":"\\x5e","{":"\\x7b","|":"\\x7c","}":"\\x7d"},eb=function(a){return cb[a]};Va[7]=function(a){return String(a).replace(bb,eb)};
Va[8]=function(a){if(null==a)return" null ";switch(typeof a){case "boolean":case "number":return" "+a+" ";default:return"'"+String(String(a)).replace(bb,eb)+"'"}};var mb=/[\x00- \x22\x27-\x29\x3c\x3e\\\x7b\x7d\x7f\x85\xa0\u2028\u2029\uff01\uff03\uff04\uff06-\uff0c\uff0f\uff1a\uff1b\uff1d\uff1f\uff20\uff3b\uff3d]/g,pb={"\x00":"%00","\u0001":"%01","\u0002":"%02","\u0003":"%03","\u0004":"%04","\u0005":"%05","\u0006":"%06","\u0007":"%07","\b":"%08","\t":"%09","\n":"%0A","\x0B":"%0B","\f":"%0C","\r":"%0D","\u000e":"%0E","\u000f":"%0F","\u0010":"%10",
"\u0011":"%11","\u0012":"%12","\u0013":"%13","\u0014":"%14","\u0015":"%15","\u0016":"%16","\u0017":"%17","\u0018":"%18","\u0019":"%19","\u001a":"%1A","\u001b":"%1B","\u001c":"%1C","\u001d":"%1D","\u001e":"%1E","\u001f":"%1F"," ":"%20",'"':"%22","'":"%27","(":"%28",")":"%29","<":"%3C",">":"%3E","\\":"%5C","{":"%7B","}":"%7D","\u007f":"%7F","\u0085":"%C2%85","\u00a0":"%C2%A0","\u2028":"%E2%80%A8","\u2029":"%E2%80%A9","\uff01":"%EF%BC%81","\uff03":"%EF%BC%83","\uff04":"%EF%BC%84","\uff06":"%EF%BC%86",
"\uff07":"%EF%BC%87","\uff08":"%EF%BC%88","\uff09":"%EF%BC%89","\uff0a":"%EF%BC%8A","\uff0b":"%EF%BC%8B","\uff0c":"%EF%BC%8C","\uff0f":"%EF%BC%8F","\uff1a":"%EF%BC%9A","\uff1b":"%EF%BC%9B","\uff1d":"%EF%BC%9D","\uff1f":"%EF%BC%9F","\uff20":"%EF%BC%A0","\uff3b":"%EF%BC%BB","\uff3d":"%EF%BC%BD"},qb=function(a){return pb[a]};Va[16]=function(a){return a};var sb;
var tb=[],ub=[],vb=[],wb=[],xb=[],yb={},zb,Ab,Bb,Cb=function(a,b){var c=a["function"];if(!c)throw Error("Error: No function name given for function call.");var d=yb[c],e={},f;for(f in a)a.hasOwnProperty(f)&&0===f.indexOf("vtp_")&&(e[void 0!==d?f:f.substr(4)]=a[f]);return void 0!==d?d(e):sb(c,e,b)},Fb=function(a,b,c){c=c||[];var d={},e;for(e in a)a.hasOwnProperty(e)&&(d[e]=Eb(a[e],b,c));return d},Gb=function(a){var b=a["function"];if(!b)throw"Error: No function name given for function call.";var c=
yb[b];return c?c.priorityOverride||0:0},Eb=function(a,b,c){if(xa(a)){var d;switch(a[0]){case "function_id":return a[1];case "list":d=[];for(var e=1;e<a.length;e++)d.push(Eb(a[e],b,c));return d;case "macro":var f=a[1];if(c[f])return;var h=tb[f];if(!h||b.Vc(h))return;c[f]=!0;try{var k=Fb(h,b,c);k.vtp_gtmEventId=b.id;d=Cb(k,b);Bb&&(d=Bb.dg(d,k))}catch(y){b.Me&&b.Me(y,Number(f)),d=!1}c[f]=!1;return d;case "map":d={};for(var l=1;l<a.length;l+=2)d[Eb(a[l],b,c)]=Eb(a[l+1],b,c);return d;case "template":d=
[];for(var n=!1,m=1;m<a.length;m++){var r=Eb(a[m],b,c);Ab&&(n=n||r===Ab.Nb);d.push(r)}return Ab&&n?Ab.gg(d):d.join("");case "escape":d=Eb(a[1],b,c);if(Ab&&xa(a[1])&&"macro"===a[1][0]&&Ab.Fg(a))return Ab.$g(d);d=String(d);for(var t=2;t<a.length;t++)Va[a[t]]&&(d=Va[a[t]](d));return d;case "tag":var q=a[1];if(!wb[q])throw Error("Unable to resolve tag reference "+q+".");return d={ye:a[2],index:q};case "zb":var u={arg0:a[2],arg1:a[3],ignore_case:a[5]};u["function"]=a[1];var w=Hb(u,b,c),v=!!a[4];return v||
2!==w?v!==(1===w):null;default:throw Error("Attempting to expand unknown Value type: "+a[0]+".");}}return a},Hb=function(a,b,c){try{return zb(Fb(a,b,c))}catch(d){JSON.stringify(a)}return 2};var Ib=function(){var a=function(b){return{toString:function(){return b}}};return{Ed:a("convert_case_to"),Fd:a("convert_false_to"),Gd:a("convert_null_to"),Hd:a("convert_true_to"),Id:a("convert_undefined_to"),Hh:a("debug_mode_metadata"),Ka:a("function"),lf:a("instance_name"),rf:a("live_only"),tf:a("malware_disabled"),uf:a("metadata"),Jh:a("original_vendor_template_id"),yf:a("once_per_event"),Sd:a("once_per_load"),$d:a("setup_tags"),be:a("tag_id"),ce:a("teardown_tags")}}();var Jb=null,Mb=function(a){function b(r){for(var t=0;t<r.length;t++)d[r[t]]=!0}var c=[],d=[];Jb=Kb(a);for(var e=0;e<ub.length;e++){var f=ub[e],h=Lb(f);if(h){for(var k=f.add||[],l=0;l<k.length;l++)c[k[l]]=!0;b(f.block||[])}else null===h&&b(f.block||[])}for(var n=[],m=0;m<wb.length;m++)c[m]&&!d[m]&&(n[m]=!0);return n},Lb=function(a){for(var b=a["if"]||[],c=0;c<b.length;c++){var d=Jb(b[c]);if(0===d)return!1;if(2===d)return null}for(var e=a.unless||[],f=0;f<e.length;f++){var h=Jb(e[f]);if(2===h)return null;
if(1===h)return!1}return!0},Kb=function(a){var b=[];return function(c){void 0===b[c]&&(b[c]=Hb(vb[c],a));return b[c]}};var Nb={dg:function(a,b){b[Ib.Ed]&&"string"===typeof a&&(a=1==b[Ib.Ed]?a.toLowerCase():a.toUpperCase());b.hasOwnProperty(Ib.Gd)&&null===a&&(a=b[Ib.Gd]);b.hasOwnProperty(Ib.Id)&&void 0===a&&(a=b[Ib.Id]);b.hasOwnProperty(Ib.Hd)&&!0===a&&(a=b[Ib.Hd]);b.hasOwnProperty(Ib.Fd)&&!1===a&&(a=b[Ib.Fd]);return a}};/*
 Copyright (c) 2014 Derek Brans, MIT license https://github.com/krux/postscribe/blob/master/LICENSE. Portions derived from simplehtmlparser, which is licensed under the Apache License, Version 2.0 */

var ic,jc=function(){};(function(){function a(k,l){k=k||"";l=l||{};for(var n in b)b.hasOwnProperty(n)&&(l.Sf&&(l["fix_"+n]=!0),l.Ae=l.Ae||l["fix_"+n]);var m={comment:/^\x3c!--/,endTag:/^<\//,atomicTag:/^<\s*(script|style|noscript|iframe|textarea)[\s\/>]/i,startTag:/^</,chars:/^[^<]/},r={comment:function(){var q=k.indexOf("--\x3e");if(0<=q)return{content:k.substr(4,q),length:q+3}},endTag:function(){var q=k.match(d);if(q)return{tagName:q[1],length:q[0].length}},atomicTag:function(){var q=r.startTag();
if(q){var u=k.slice(q.length);if(u.match(new RegExp("</\\s*"+q.tagName+"\\s*>","i"))){var w=u.match(new RegExp("([\\s\\S]*?)</\\s*"+q.tagName+"\\s*>","i"));if(w)return{tagName:q.tagName,S:q.S,content:w[1],length:w[0].length+q.length}}}},startTag:function(){var q=k.match(c);if(q){var u={};q[2].replace(e,function(w,v,y,x,A){var B=y||x||A||f.test(v)&&v||null,z=document.createElement("div");z.innerHTML=B;u[v]=z.textContent||z.innerText||B});return{tagName:q[1],S:u,Gb:!!q[3],length:q[0].length}}},chars:function(){var q=
k.indexOf("<");return{length:0<=q?q:k.length}}},t=function(){for(var q in m)if(m[q].test(k)){var u=r[q]();return u?(u.type=u.type||q,u.text=k.substr(0,u.length),k=k.slice(u.length),u):null}};l.Ae&&function(){var q=/^(AREA|BASE|BASEFONT|BR|COL|FRAME|HR|IMG|INPUT|ISINDEX|LINK|META|PARAM|EMBED)$/i,u=/^(COLGROUP|DD|DT|LI|OPTIONS|P|TD|TFOOT|TH|THEAD|TR)$/i,w=[];w.Je=function(){return this[this.length-1]};w.Xc=function(z){var D=this.Je();return D&&D.tagName&&D.tagName.toUpperCase()===z.toUpperCase()};w.cg=
function(z){for(var D=0,F;F=this[D];D++)if(F.tagName===z)return!0;return!1};var v=function(z){z&&"startTag"===z.type&&(z.Gb=q.test(z.tagName)||z.Gb);return z},y=t,x=function(){k="</"+w.pop().tagName+">"+k},A={startTag:function(z){var D=z.tagName;"TR"===D.toUpperCase()&&w.Xc("TABLE")?(k="<TBODY>"+k,B()):l.Th&&u.test(D)&&w.cg(D)?w.Xc(D)?x():(k="</"+z.tagName+">"+k,B()):z.Gb||w.push(z)},endTag:function(z){w.Je()?l.og&&!w.Xc(z.tagName)?x():w.pop():l.og&&(y(),B())}},B=function(){var z=k,D=v(y());k=z;if(D&&
A[D.type])A[D.type](D)};t=function(){B();return v(y())}}();return{append:function(q){k+=q},ih:t,Zh:function(q){for(var u;(u=t())&&(!q[u.type]||!1!==q[u.type](u)););},clear:function(){var q=k;k="";return q},$h:function(){return k},stack:[]}}var b=function(){var k={},l=this.document.createElement("div");l.innerHTML="<P><I></P></I>";k.di="<P><I></P></I>"!==l.innerHTML;l.innerHTML="<P><i><P></P></i></P>";k.ci=2===l.childNodes.length;return k}(),c=/^<([\-A-Za-z0-9_]+)((?:\s+[\w\-]+(?:\s*=?\s*(?:(?:"[^"]*")|(?:'[^']*')|[^>\s]+))?)*)\s*(\/?)>/,
d=/^<\/([\-A-Za-z0-9_]+)[^>]*>/,e=/([\-A-Za-z0-9_]+)(?:\s*=\s*(?:(?:"((?:\\.|[^"])*)")|(?:'((?:\\.|[^'])*)')|([^>\s]+)))?/g,f=/^(checked|compact|declare|defer|disabled|ismap|multiple|nohref|noresize|noshade|nowrap|readonly|selected)$/i;a.o=b;a.H=function(k){var l={comment:function(n){return"<--"+n.content+"--\x3e"},endTag:function(n){return"</"+n.tagName+">"},atomicTag:function(n){return l.startTag(n)+n.content+l.endTag(n)},startTag:function(n){var m="<"+n.tagName,r;for(r in n.S){var t=n.S[r];m+=
" "+r+'="'+(t?t.replace(/(^|[^\\])"/g,'$1\\"'):"")+'"'}return m+(n.Gb?"/>":">")},chars:function(n){return n.text}};return l[k.type](k)};a.i=function(k){var l={},n;for(n in k){var m=k[n];l[n]=m&&m.replace(/(^|[^\\])"/g,'$1\\"')}return l};for(var h in b)a.h=a.h||!b[h]&&h;ic=a})();(function(){function a(){}function b(r){return void 0!==r&&null!==r}function c(r,t,q){var u,w=r&&r.length||0;for(u=0;u<w;u++)t.call(q,r[u],u)}function d(r,t,q){for(var u in r)r.hasOwnProperty(u)&&t.call(q,u,r[u])}function e(r,
t){d(t,function(q,u){r[q]=u});return r}function f(r,t){r=r||{};d(t,function(q,u){b(r[q])||(r[q]=u)});return r}function h(r){try{return n.call(r)}catch(q){var t=[];c(r,function(u){t.push(u)});return t}}var k={Jf:a,Kf:a,Lf:a,Mf:a,Tf:a,Uf:function(r){return r},done:a,error:function(r){throw r;},lh:!1},l=this;if(!l.postscribe){var n=Array.prototype.slice,m=function(){function r(q,u,w){var v="data-ps-"+u;if(2===arguments.length){var y=q.getAttribute(v);return b(y)?String(y):y}b(w)&&""!==w?q.setAttribute(v,
w):q.removeAttribute(v)}function t(q,u){var w=q.ownerDocument;e(this,{root:q,options:u,Hb:w.defaultView||w.parentWindow,Sa:w,nc:ic("",{Sf:!0}),Jc:[q],hd:"",jd:w.createElement(q.nodeName),Db:[],Ia:[]});r(this.jd,"proxyof",0)}t.prototype.write=function(){[].push.apply(this.Ia,arguments);for(var q;!this.Zb&&this.Ia.length;)q=this.Ia.shift(),"function"===typeof q?this.Zf(q):this.xd(q)};t.prototype.Zf=function(q){var u={type:"function",value:q.name||q.toString()};this.dd(u);q.call(this.Hb,this.Sa);this.Ne(u)};
t.prototype.xd=function(q){this.nc.append(q);for(var u,w=[],v,y;(u=this.nc.ih())&&!(v=u&&"tagName"in u?!!~u.tagName.toLowerCase().indexOf("script"):!1)&&!(y=u&&"tagName"in u?!!~u.tagName.toLowerCase().indexOf("style"):!1);)w.push(u);this.Ch(w);v&&this.yg(u);y&&this.zg(u)};t.prototype.Ch=function(q){var u=this.Wf(q);u.pe&&(u.Tc=this.hd+u.pe,this.hd+=u.eh,this.jd.innerHTML=u.Tc,this.zh())};t.prototype.Wf=function(q){var u=this.Jc.length,w=[],v=[],y=[];c(q,function(x){w.push(x.text);if(x.S){if(!/^noscript$/i.test(x.tagName)){var A=
u++;v.push(x.text.replace(/(\/?>)/," data-ps-id="+A+" $1"));"ps-script"!==x.S.id&&"ps-style"!==x.S.id&&y.push("atomicTag"===x.type?"":"<"+x.tagName+" data-ps-proxyof="+A+(x.Gb?" />":">"))}}else v.push(x.text),y.push("endTag"===x.type?x.text:"")});return{ei:q,raw:w.join(""),pe:v.join(""),eh:y.join("")}};t.prototype.zh=function(){for(var q,u=[this.jd];b(q=u.shift());){var w=1===q.nodeType;if(!w||!r(q,"proxyof")){w&&(this.Jc[r(q,"id")]=q,r(q,"id",null));var v=q.parentNode&&r(q.parentNode,"proxyof");
v&&this.Jc[v].appendChild(q)}u.unshift.apply(u,h(q.childNodes))}};t.prototype.yg=function(q){var u=this.nc.clear();u&&this.Ia.unshift(u);q.src=q.S.src||q.S.Mh;q.src&&this.Db.length?this.Zb=q:this.dd(q);var w=this;this.Bh(q,function(){w.Ne(q)})};t.prototype.zg=function(q){var u=this.nc.clear();u&&this.Ia.unshift(u);q.type=q.S.type||q.S.TYPE||"text/css";this.Dh(q);u&&this.write()};t.prototype.Dh=function(q){var u=this.Yf(q);this.Cg(u);q.content&&(u.styleSheet&&!u.sheet?u.styleSheet.cssText=q.content:
u.appendChild(this.Sa.createTextNode(q.content)))};t.prototype.Yf=function(q){var u=this.Sa.createElement(q.tagName);u.setAttribute("type",q.type);d(q.S,function(w,v){u.setAttribute(w,v)});return u};t.prototype.Cg=function(q){this.xd('<span id="ps-style"/>');var u=this.Sa.getElementById("ps-style");u.parentNode.replaceChild(q,u)};t.prototype.dd=function(q){q.Vg=this.Ia;this.Ia=[];this.Db.unshift(q)};t.prototype.Ne=function(q){q!==this.Db[0]?this.options.error({message:"Bad script nesting or script finished twice"}):
(this.Db.shift(),this.write.apply(this,q.Vg),!this.Db.length&&this.Zb&&(this.dd(this.Zb),this.Zb=null))};t.prototype.Bh=function(q,u){var w=this.Xf(q),v=this.rh(w),y=this.options.Jf;q.src&&(w.src=q.src,this.ph(w,v?y:function(){u();y()}));try{this.Bg(w),q.src&&!v||u()}catch(x){this.options.error(x),u()}};t.prototype.Xf=function(q){var u=this.Sa.createElement(q.tagName);d(q.S,function(w,v){u.setAttribute(w,v)});q.content&&(u.text=q.content);return u};t.prototype.Bg=function(q){this.xd('<span id="ps-script"/>');
var u=this.Sa.getElementById("ps-script");u.parentNode.replaceChild(q,u)};t.prototype.ph=function(q,u){function w(){q=q.onload=q.onreadystatechange=q.onerror=null}var v=this.options.error;e(q,{onload:function(){w();u()},onreadystatechange:function(){/^(loaded|complete)$/.test(q.readyState)&&(w(),u())},onerror:function(){var y={message:"remote script failed "+q.src};w();v(y);u()}})};t.prototype.rh=function(q){return!/^script$/i.test(q.nodeName)||!!(this.options.lh&&q.src&&q.hasAttribute("async"))};
return t}();l.postscribe=function(){function r(){var v=u.shift(),y;v&&(y=v[v.length-1],y.Kf(),v.stream=t.apply(null,v),y.Lf())}function t(v,y,x){function A(F){F=x.Uf(F);w.write(F);x.Mf(F)}w=new m(v,x);w.id=q++;w.name=x.name||w.id;var B=v.ownerDocument,z={close:B.close,open:B.open,write:B.write,writeln:B.writeln};e(B,{close:a,open:a,write:function(){return A(h(arguments).join(""))},writeln:function(){return A(h(arguments).join("")+"\n")}});var D=w.Hb.onerror||a;w.Hb.onerror=function(F,L,O){x.error({Xh:F+
" - "+L+":"+O});D.apply(w.Hb,arguments)};w.write(y,function(){e(B,z);w.Hb.onerror=D;x.done();w=null;r()});return w}var q=0,u=[],w=null;return e(function(v,y,x){"function"===typeof x&&(x={done:x});x=f(x,k);v=/^#/.test(v)?l.document.getElementById(v.substr(1)):v.Vh?v[0]:v;var A=[v,y,x];v.Zg={cancel:function(){A.stream?A.stream.abort():A[1]=a}};x.Tf(A);u.push(A);w||r();return v.Zg},{streams:{},Yh:u,Oh:m})}();jc=l.postscribe}})();var C={cb:"_ee",Hc:"_syn",Nh:"_uei",Kh:"_pci",Dc:"event_callback",Mb:"event_timeout",ca:"gtag.config",da:"allow_ad_personalization_signals",Ec:"restricted_data_processing",$a:"allow_google_signals",fa:"cookie_expires",Lb:"cookie_update",ab:"session_duration",la:"user_properties",za:"transport_url",O:"ads_data_redaction",m:"ad_storage",J:"analytics_storage",af:"region",bf:"wait_for_update"};C.xe=[C.da,C.$a,C.Lb];
C.De=[C.fa,C.Mb,C.ab];var kc={},lc=function(a,b){kc[a]=kc[a]||[];kc[a][b]=!0},mc=function(a){for(var b=[],c=kc[a]||[],d=0;d<c.length;d++)c[d]&&(b[Math.floor(d/6)]^=1<<d%6);for(var e=0;e<b.length;e++)b[e]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".charAt(b[e]||0);return b.join("")};var E=function(a){lc("GTM",a)};var nc=function(a){if(Error.captureStackTrace)Error.captureStackTrace(this,nc);else{var b=Error().stack;b&&(this.stack=b)}a&&(this.message=String(a))};qa(nc,Error);nc.prototype.name="CustomError";var oc=function(a,b){for(var c=a.split("%s"),d="",e=c.length-1,f=0;f<e;f++)d+=c[f]+(f<b.length?b[f]:"%s");nc.call(this,d+c[e])};qa(oc,nc);oc.prototype.name="AssertionError";var pc=function(a,b){throw new oc("Failure"+(a?": "+a:""),Array.prototype.slice.call(arguments,1));};var qc=function(a,b){var c=function(){};c.prototype=a.prototype;var d=new c;a.apply(d,Array.prototype.slice.call(arguments,1));return d};var rc;var sc=/^(?:(?:https?|mailto|ftp):|[^:/?#]*(?:[/?#]|$))/i;var tc;a:{var uc=ma.navigator;if(uc){var vc=uc.userAgent;if(vc){tc=vc;break a}}tc=""}var wc=function(a){return-1!=tc.indexOf(a)};var yc=function(){this.h="";this.i=xc};yc.prototype.toString=function(){return"SafeHtml{"+this.h+"}"};var zc=function(a){if(a instanceof yc&&a.constructor===yc&&a.i===xc)return a.h;var b=typeof a;pc("expected object of type SafeHtml, got '"+a+"' of type "+("object"!=b?b:a?Array.isArray(a)?"array":b:"null"));return"type_error:SafeHtml"},xc={},Ac=new yc;Ac.h=ma.trustedTypes&&ma.trustedTypes.emptyHTML?ma.trustedTypes.emptyHTML:"";var Bc={MATH:!0,SCRIPT:!0,STYLE:!0,SVG:!0,TEMPLATE:!0},Ec=function(a){var b=!1,c;return function(){b||(c=a(),b=!0);return c}}(function(){if("undefined"===typeof document)return!1;var a=document.createElement("div"),b=document.createElement("div");b.appendChild(document.createElement("div"));a.appendChild(b);if(!a.firstChild)return!1;var c=a.firstChild.firstChild;a.innerHTML=zc(Ac);return!c.parentElement}),Fc=function(a,b){if(Bc[a.tagName.toUpperCase()])throw Error("goog.dom.safe.setInnerHtml cannot be used to set content of "+
a.tagName+".");if(Ec())for(;a.lastChild;)a.removeChild(a.lastChild);a.innerHTML=zc(b)};var Gc=function(a){var b=new yc,c;if(void 0===rc){var d=null,e=ma.trustedTypes;if(e&&e.createPolicy){try{d=e.createPolicy("goog#html",{createHTML:ra,createScript:ra,createScriptURL:ra})}catch(f){ma.console&&ma.console.error(f.message)}rc=d}else rc=d}c=rc;b.h=c?c.createHTML(a):a;return b};var G=window,H=document,Hc=navigator,Ic=H.currentScript&&H.currentScript.src,Jc=function(a,b){var c=G[a];G[a]=void 0===c?b:c;return G[a]},Kc=function(a,b){b&&(a.addEventListener?a.onload=b:a.onreadystatechange=function(){a.readyState in{loaded:1,complete:1}&&(a.onreadystatechange=null,b())})},Lc=function(a,b,c){var d=H.createElement("script");d.type="text/javascript";d.async=!0;d.src=a;Kc(d,b);c&&(d.onerror=c);var e;if(null===pa)b:{var f=ma.document,h=f.querySelector&&f.querySelector("script[nonce]");
if(h){var k=h.nonce||h.getAttribute("nonce");if(k&&oa.test(k)){pa=k;break b}}pa=""}e=pa;e&&d.setAttribute("nonce",e);var l=H.getElementsByTagName("script")[0]||H.body||H.head;l.parentNode.insertBefore(d,l);return d},Mc=function(){if(Ic){var a=Ic.toLowerCase();if(0===a.indexOf("https://"))return 2;if(0===a.indexOf("http://"))return 3}return 1},Nc=function(a,b){var c=H.createElement("iframe");c.height="0";c.width="0";c.style.display="none";c.style.visibility="hidden";var d=H.body&&H.body.lastChild||
H.body||H.head;d.parentNode.insertBefore(c,d);Kc(c,b);void 0!==a&&(c.src=a);return c},Oc=function(a,b,c){var d=new Image(1,1);d.onload=function(){d.onload=null;b&&b()};d.onerror=function(){d.onerror=null;c&&c()};d.src=a;return d},Pc=function(a,b,c,d){a.addEventListener?a.addEventListener(b,c,!!d):a.attachEvent&&a.attachEvent("on"+b,c)},Qc=function(a,b,c){a.removeEventListener?a.removeEventListener(b,c,!1):a.detachEvent&&a.detachEvent("on"+b,c)},I=function(a){G.setTimeout(a,0)},Rc=function(a,b){return a&&
b&&a.attributes&&a.attributes[b]?a.attributes[b].value:null},Sc=function(a){var b=a.innerText||a.textContent||"";b&&" "!=b&&(b=b.replace(/^[\s\xa0]+|[\s\xa0]+$/g,""));b&&(b=b.replace(/(\xa0+|\s{2,}|\n|\r\t)/g," "));return b},Tc=function(a){var b=H.createElement("div");Fc(b,Gc("A<div>"+a+"</div>"));b=b.lastChild;for(var c=[];b.firstChild;)c.push(b.removeChild(b.firstChild));return c},Uc=function(a,b,c){c=c||100;for(var d={},e=0;e<b.length;e++)d[b[e]]=!0;for(var f=a,h=0;f&&h<=c;h++){if(d[String(f.tagName).toLowerCase()])return f;
f=f.parentElement}return null},Vc=function(a){Hc.sendBeacon&&Hc.sendBeacon(a)||Oc(a)},Wc=function(a,b){var c=a[b];c&&"string"===typeof c.animVal&&(c=c.animVal);return c};var Xc={},Yc=function(a){return void 0==Xc[a]?!1:Xc[a]};var Zc=[];function $c(){var a=Jc("google_tag_data",{});a.ics||(a.ics={entries:{},set:ad,update:bd,addListener:cd,notifyListeners:dd,active:!1});return a.ics}
function ad(a,b,c,d,e,f){var h=$c();h.active=!0;if(void 0!=b){var k=h.entries,l=k[a]||{},n=l.region,m=c&&g(c)?c.toUpperCase():void 0;d=d.toUpperCase();e=e.toUpperCase();if(m===e||(m===d?n!==e:!m&&!n)){var r=!!(f&&0<f&&void 0===l.update),t={region:m,initial:"granted"===b,update:l.update,quiet:r};k[a]=t;r&&G.setTimeout(function(){k[a]===t&&t.quiet&&(t.quiet=!1,ed(a),dd(),lc("TAGGING",2))},f)}}}
function bd(a,b){var c=$c();c.active=!0;if(void 0!=b){var d=fd(a),e=c.entries,f=e[a]=e[a]||{};f.update="granted"===b;var h=fd(a);f.quiet?(f.quiet=!1,ed(a)):h!==d&&ed(a)}}function cd(a,b){Zc.push({te:a,pg:b})}function ed(a){for(var b=0;b<Zc.length;++b){var c=Zc[b];xa(c.te)&&-1!==c.te.indexOf(a)&&(c.Qe=!0)}}function dd(){for(var a=0;a<Zc.length;++a){var b=Zc[a];if(b.Qe){b.Qe=!1;try{b.pg.call()}catch(c){}}}}
var fd=function(a){var b=$c().entries[a]||{};return void 0!==b.update?b.update:void 0!==b.initial?b.initial:void 0},gd=function(a){return!($c().entries[a]||{}).quiet},hd=function(){return Yc("gtag_cs_api")?$c().active:!1},id=function(a,b){$c().addListener(a,b)},jd=function(a,b){function c(){for(var e=0;e<b.length;e++)if(!gd(b[e]))return!0;return!1}if(c()){var d=!1;id(b,function(){d||c()||(d=!0,a())})}else a()},kd=function(a,b){if(!1===fd(b)){var c=!1;id([b],function(){!c&&fd(b)&&(a(),c=!0)})}};var ld=[C.m,C.J],md=function(a){var b=a[C.af];b&&E(40);var c=a[C.bf];c&&E(41);for(var d=xa(b)?b:[b],e=0;e<d.length;++e)for(var f=0;f<ld.length;f++){var h=ld[f],k=a[ld[f]],l=d[e];$c().set(h,k,l,"US","US-VA",c)}},nd=function(a){for(var b=0;b<ld.length;b++){var c=ld[b],d=a[ld[b]];$c().update(c,d)}$c().notifyListeners()},od=function(a){var b=fd(a);return void 0!=b?b:!0},pd=function(){for(var a=[],b=0;b<ld.length;b++){var c=fd(ld[b]);a[b]=!0===c?"1":!1===c?"0":"-"}return"G1"+a.join("")},
qd=function(a,b){jd(a,b)};var sd=function(a){return rd?H.querySelectorAll(a):null},td=function(a,b){if(!rd)return null;if(Element.prototype.closest)try{return a.closest(b)}catch(e){return null}var c=Element.prototype.matches||Element.prototype.webkitMatchesSelector||Element.prototype.mozMatchesSelector||Element.prototype.msMatchesSelector||Element.prototype.oMatchesSelector,d=a;if(!H.documentElement.contains(d))return null;do{try{if(c.call(d,b))return d}catch(e){break}d=d.parentElement||d.parentNode}while(null!==d&&1===d.nodeType);
return null},xd=!1;if(H.querySelectorAll)try{var yd=H.querySelectorAll(":root");yd&&1==yd.length&&yd[0]==H.documentElement&&(xd=!0)}catch(a){}var rd=xd;var Md={},K=null,Nd=Math.random();Md.C="GTM-L8ZB";Md.Rb="871";Md.Ih="";var Od={__cl:!0,__ecl:!0,__ehl:!0,__evl:!0,__fal:!0,__fil:!0,__fsl:!0,__hl:!0,__jel:!0,__lcl:!0,__sdl:!0,__tl:!0,__ytl:!0,__paused:!0,__tg:!0},Pd="www.googletagmanager.com/gtm.js";
var Qd=Pd,Rd=null,Sd=null,Td="//www.googletagmanager.com/a?id="+Md.C+"&cv=206",Ud={},Vd={},Wd=function(){var a=K.sequence||1;K.sequence=a+1;return a};
var Xd=function(){return"&tc="+wb.filter(function(a){return a}).length},$d=function(){Yd||(Yd=G.setTimeout(Zd,500))},Zd=function(){Yd&&(G.clearTimeout(Yd),Yd=void 0);void 0===ae||be[ae]&&!ce&&!de||(ee[ae]||fe.Hg()||0>=ge--?(E(1),ee[ae]=!0):(fe.jh(),Oc(he()),be[ae]=!0,ie=je=de=ce=""))},he=function(){var a=ae;if(void 0===a)return"";var b=mc("GTM"),c=mc("TAGGING");return[ke,be[a]?"":"&es=1",le[a],b?"&u="+b:"",c?"&ut="+c:"",Xd(),ce,de,je,ie,"&z=0"].join("")},me=function(){return[Td,"&v=3&t=t","&pid="+
Aa(),"&rv="+Md.Rb].join("")},ne="0.005000">Math.random(),ke=me(),oe=function(){ke=me()},be={},ce="",de="",ie="",je="",ae=void 0,le={},ee={},Yd=void 0,fe=function(a,b){var c=0,d=0;return{Hg:function(){if(c<a)return!1;Ia()-d>=b&&(c=0);return c>=a},jh:function(){Ia()-d>=b&&(c=0);c++;d=Ia()}}}(2,1E3),ge=1E3,pe=function(a,b){if(ne&&!ee[a]&&ae!==a){Zd();ae=a;ie=ce="";var c;c=0===b.indexOf("gtm.")?encodeURIComponent(b):"*";le[a]="&e="+c+"&eid="+a;$d()}},qe=function(a,b,c){if(ne&&!ee[a]&&
b){a!==ae&&(Zd(),ae=a);var d,e=String(b[Ib.Ka]||"").replace(/_/g,"");0===e.indexOf("cvt")&&(e="cvt");d=e;var f=c+d;ce=ce?ce+"."+f:"&tr="+f;var h=b["function"];if(!h)throw Error("Error: No function name given for function call.");var k=(yb[h]?"1":"2")+d;ie=ie?ie+"."+k:"&ti="+k;$d();2022<=he().length&&Zd()}},re=function(a,b,c){if(ne&&!ee[a]){a!==ae&&(Zd(),ae=a);var d=c+b;de=de?de+
"."+d:"&epr="+d;$d();2022<=he().length&&Zd()}};var se={},te=new Ba,ue={},ve={},ye={name:"dataLayer",set:function(a,b){p(Oa(a,b),ue);we()},get:function(a){return xe(a,2)},reset:function(){te=new Ba;ue={};we()}},xe=function(a,b){if(2!=b){var c=te.get(a);ne&&c!==ze(a.split("."))&&E(5);return c}return ze(a.split("."))},ze=function(a){for(var b=ue,c=0;c<a.length;c++){if(null===b)return!1;if(void 0===b)break;b=b[a[c]]}return b},Ae=function(a,b){ve.hasOwnProperty(a)||(te.set(a,b),p(Oa(a,b),ue),we())},we=function(a){Da(ve,function(b,c){te.set(b,c);
p(Oa(b,void 0),ue);p(Oa(b,c),ue);a&&delete ve[b]})},Be=function(a,b,c){se[a]=se[a]||{};var d=1!==c?ze(b.split(".")):te.get(b);"array"===Ra(d)||"object"===Ra(d)?se[a][b]=p(d):se[a][b]=d},Ce=function(a,b){if(se[a])return se[a][b]},De=function(a,b){se[a]&&delete se[a][b]};var Ge=/:[0-9]+$/,He=function(a,b,c){for(var d=a.split("&"),e=0;e<d.length;e++){var f=d[e].split("=");if(decodeURIComponent(f[0]).replace(/\+/g," ")===b){var h=f.slice(1).join("=");return c?h:decodeURIComponent(h).replace(/\+/g," ")}}},Ke=function(a,b,c,d,e){b&&(b=String(b).toLowerCase());if("protocol"===b||"port"===b)a.protocol=Ie(a.protocol)||Ie(G.location.protocol);"port"===b?a.port=String(Number(a.hostname?a.port:G.location.port)||("http"==a.protocol?80:"https"==a.protocol?443:"")):"host"===b&&
(a.hostname=(a.hostname||G.location.hostname).replace(Ge,"").toLowerCase());return Je(a,b,c,d,e)},Je=function(a,b,c,d,e){var f,h=Ie(a.protocol);b&&(b=String(b).toLowerCase());switch(b){case "url_no_fragment":f=Le(a);break;case "protocol":f=h;break;case "host":f=a.hostname.replace(Ge,"").toLowerCase();if(c){var k=/^www\d*\./.exec(f);k&&k[0]&&(f=f.substr(k[0].length))}break;case "port":f=String(Number(a.port)||("http"==h?80:"https"==h?443:""));break;case "path":a.pathname||a.hostname||lc("TAGGING",
1);f="/"==a.pathname.substr(0,1)?a.pathname:"/"+a.pathname;var l=f.split("/");0<=ya(d||[],l[l.length-1])&&(l[l.length-1]="");f=l.join("/");break;case "query":f=a.search.replace("?","");e&&(f=He(f,e,void 0));break;case "extension":var n=a.pathname.split(".");f=1<n.length?n[n.length-1]:"";f=f.split("/")[0];break;case "fragment":f=a.hash.replace("#","");break;default:f=a&&a.href}return f},Ie=function(a){return a?a.replace(":","").toLowerCase():""},Le=function(a){var b="";if(a&&a.href){var c=a.href.indexOf("#");
b=0>c?a.href:a.href.substr(0,c)}return b},Me=function(a){var b=H.createElement("a");a&&(b.href=a);var c=b.pathname;"/"!==c[0]&&(a||lc("TAGGING",1),c="/"+c);var d=b.hostname.replace(Ge,"");return{href:b.href,protocol:b.protocol,host:b.host,hostname:d,pathname:c,search:b.search,hash:b.hash,port:b.port}},Ne=function(a){function b(n){var m=n.split("=")[0];return 0>d.indexOf(m)?n:m+"=0"}function c(n){return n.split("&").map(b).filter(function(m){return void 0!=m}).join("&")}var d="gclid dclid gclaw gcldc gclgp gclha gclgf _gl".split(" "),
e=Me(a),f=a.split(/[?#]/)[0],h=e.search,k=e.hash;"?"===h[0]&&(h=h.substring(1));"#"===k[0]&&(k=k.substring(1));h=c(h);k=c(k);""!==h&&(h="?"+h);""!==k&&(k="#"+k);var l=""+f+h+k;"/"===l[l.length-1]&&(l=l.substring(0,l.length-1));return l};function Oe(a,b,c){for(var d=[],e=b.split(";"),f=0;f<e.length;f++){var h=e[f].split("="),k=h[0].replace(/^\s*|\s*$/g,"");if(k&&k==a){var l=h.slice(1).join("=").replace(/^\s*|\s*$/g,"");l&&c&&(l=decodeURIComponent(l));d.push(l)}}return d};var Qe=function(a,b,c,d){return Pe(d)?Oe(a,String(b||document.cookie),c):[]},Te=function(a,b,c,d,e){if(Pe(e)){var f=Re(a,d,e);if(1===f.length)return f[0].id;if(0!==f.length){f=Se(f,function(h){return h.$b},b);if(1===f.length)return f[0].id;f=Se(f,function(h){return h.Bb},c);return f[0]?f[0].id:void 0}}};function Ue(a,b,c,d){var e=document.cookie;document.cookie=a;var f=document.cookie;return e!=f||void 0!=c&&0<=Qe(b,f,!1,d).indexOf(c)}
var Ye=function(a,b,c){function d(q,u,w){if(null==w)return delete h[u],q;h[u]=w;return q+"; "+u+"="+w}function e(q,u){if(null==u)return delete h[u],q;h[u]=!0;return q+"; "+u}if(!Pe(c.Fa))return 2;var f;void 0==b?f=a+"=deleted; expires="+(new Date(0)).toUTCString():(c.encode&&(b=encodeURIComponent(b)),b=Ve(b),f=a+"="+b);var h={};f=d(f,"path",c.path);var k;c.expires instanceof Date?k=c.expires.toUTCString():null!=c.expires&&(k=""+c.expires);f=d(f,"expires",k);f=d(f,"max-age",c.Wh);f=d(f,"samesite",
c.ai);c.bi&&(f=e(f,"secure"));var l=c.domain;if("auto"===l){for(var n=We(),m=0;m<n.length;++m){var r="none"!==n[m]?n[m]:void 0,t=d(f,"domain",r);t=e(t,c.flags);if(!Xe(r,c.path)&&Ue(t,a,b,c.Fa))return 0}return 1}l&&"none"!==l&&(f=d(f,"domain",l));f=e(f,c.flags);return Xe(l,c.path)?1:Ue(f,a,b,c.Fa)?0:1},Ze=function(a,b,c){null==c.path&&(c.path="/");c.domain||(c.domain="auto");return Ye(a,b,c)};
function Se(a,b,c){for(var d=[],e=[],f,h=0;h<a.length;h++){var k=a[h],l=b(k);l===c?d.push(k):void 0===f||l<f?(e=[k],f=l):l===f&&e.push(k)}return 0<d.length?d:e}function Re(a,b,c){for(var d=[],e=Qe(a,void 0,void 0,c),f=0;f<e.length;f++){var h=e[f].split("."),k=h.shift();if(!b||-1!==b.indexOf(k)){var l=h.shift();l&&(l=l.split("-"),d.push({id:h.join("."),$b:1*l[0]||1,Bb:1*l[1]||1}))}}return d}
var Ve=function(a){a&&1200<a.length&&(a=a.substring(0,1200));return a},$e=/^(www\.)?google(\.com?)?(\.[a-z]{2})?$/,af=/(^|\.)doubleclick\.net$/i,Xe=function(a,b){return af.test(document.location.hostname)||"/"===b&&$e.test(a)},We=function(){var a=[],b=document.location.hostname.split(".");if(4===b.length){var c=b[b.length-1];if(parseInt(c,10).toString()===c)return["none"]}for(var d=b.length-2;0<=d;d--)a.push(b.slice(d).join("."));var e=document.location.hostname;af.test(e)||$e.test(e)||a.push("none");
return a},Pe=function(a){if(!Yc("gtag_cs_api")||!a||!hd())return!0;if(!gd(a))return!1;var b=fd(a);return null==b?!0:!!b};var bf=function(){for(var a=Hc.userAgent+(H.cookie||"")+(H.referrer||""),b=a.length,c=G.history.length;0<c;)a+=c--^b++;var d=1,e,f,h;if(a)for(d=0,f=a.length-1;0<=f;f--)h=a.charCodeAt(f),d=(d<<6&268435455)+h+(h<<14),e=d&266338304,d=0!=e?d^e>>21:d;return[Math.round(2147483647*Math.random())^d&2147483647,Math.round(Ia()/1E3)].join(".")},ef=function(a,b,c,d,e){var f=cf(b);return Te(a,f,df(c),d,e)},ff=function(a,b,c,d){var e=""+cf(c),f=df(d);1<f&&(e+="-"+f);return[b,e,a].join(".")},cf=function(a){if(!a)return 1;
a=0===a.indexOf(".")?a.substr(1):a;return a.split(".").length},df=function(a){if(!a||"/"===a)return 1;"/"!==a[0]&&(a="/"+a);"/"!==a[a.length-1]&&(a+="/");return a.split("/").length-1};function gf(a,b,c){var d,e=a.zb;null==e&&(e=7776E3);0!==e&&(d=new Date((b||Ia())+1E3*e));return{path:a.path,domain:a.domain,flags:a.flags,encode:!!c,expires:d}};var hf=["1"],jf={},nf=function(a){var b=kf(a.prefix);jf[b]||lf(b,a.path,a.domain)||(mf(b,bf(),a),lf(b,a.path,a.domain))};function mf(a,b,c){var d=ff(b,"1",c.domain,c.path),e=gf(c);e.Fa="ad_storage";Ze(a,d,e)}function lf(a,b,c){var d=ef(a,b,c,hf,"ad_storage");d&&(jf[a]=d);return d}function kf(a){return(a||"_gcl")+"_au"};function of(){for(var a=pf,b={},c=0;c<a.length;++c)b[a[c]]=c;return b}function qf(){var a="ABCDEFGHIJKLMNOPQRSTUVWXYZ";a+=a.toLowerCase()+"0123456789-_";return a+"."}var pf,rf;function sf(a){pf=pf||qf();rf=rf||of();for(var b=[],c=0;c<a.length;c+=3){var d=c+1<a.length,e=c+2<a.length,f=a.charCodeAt(c),h=d?a.charCodeAt(c+1):0,k=e?a.charCodeAt(c+2):0,l=f>>2,n=(f&3)<<4|h>>4,m=(h&15)<<2|k>>6,r=k&63;e||(r=64,d||(m=64));b.push(pf[l],pf[n],pf[m],pf[r])}return b.join("")}
function tf(a){function b(l){for(;d<a.length;){var n=a.charAt(d++),m=rf[n];if(null!=m)return m;if(!/^[\s\xa0]*$/.test(n))throw Error("Unknown base64 encoding at char: "+n);}return l}pf=pf||qf();rf=rf||of();for(var c="",d=0;;){var e=b(-1),f=b(0),h=b(64),k=b(64);if(64===k&&-1===e)return c;c+=String.fromCharCode(e<<2|f>>4);64!=h&&(c+=String.fromCharCode(f<<4&240|h>>2),64!=k&&(c+=String.fromCharCode(h<<6&192|k)))}};var uf;var yf=function(){var a=vf,b=wf,c=xf(),d=function(h){a(h.target||h.srcElement||{})},e=function(h){b(h.target||h.srcElement||{})};if(!c.init){Pc(H,"mousedown",d);Pc(H,"keyup",d);Pc(H,"submit",e);var f=HTMLFormElement.prototype.submit;HTMLFormElement.prototype.submit=function(){b(this);f.call(this)};c.init=!0}},zf=function(a,b,c,d,e){var f={callback:a,domains:b,fragment:2===c,placement:c,forms:d,sameHost:e};xf().decorators.push(f)},Af=function(a,b,c){for(var d=xf().decorators,e={},f=0;f<d.length;++f){var h=
d[f],k;if(k=!c||h.forms)a:{var l=h.domains,n=a,m=!!h.sameHost;if(l&&(m||n!==H.location.hostname))for(var r=0;r<l.length;r++)if(l[r]instanceof RegExp){if(l[r].test(n)){k=!0;break a}}else if(0<=n.indexOf(l[r])||m&&0<=l[r].indexOf(n)){k=!0;break a}k=!1}if(k){var t=h.placement;void 0==t&&(t=h.fragment?2:1);t===b&&La(e,h.callback())}}return e},xf=function(){var a=Jc("google_tag_data",{}),b=a.gl;b&&b.decorators||(b={decorators:[]},a.gl=b);return b};var Bf=/(.*?)\*(.*?)\*(.*)/,Cf=/^https?:\/\/([^\/]*?)\.?cdn\.ampproject\.org\/?(.*)/,Df=/^(?:www\.|m\.|amp\.)+/,Ef=/([^?#]+)(\?[^#]*)?(#.*)?/;function Ff(a){return new RegExp("(.*?)(^|&)"+a+"=([^&]*)&?(.*)")}
var Hf=function(a){var b=[],c;for(c in a)if(a.hasOwnProperty(c)){var d=a[c];void 0!==d&&d===d&&null!==d&&"[object Object]"!==d.toString()&&(b.push(c),b.push(sf(String(d))))}var e=b.join("*");return["1",Gf(e),e].join("*")},Gf=function(a,b){var c=[window.navigator.userAgent,(new Date).getTimezoneOffset(),window.navigator.userLanguage||window.navigator.language,Math.floor((new Date).getTime()/60/1E3)-(void 0===b?0:b),a].join("*"),d;if(!(d=uf)){for(var e=Array(256),f=0;256>f;f++){for(var h=f,k=0;8>k;k++)h=
h&1?h>>>1^3988292384:h>>>1;e[f]=h}d=e}uf=d;for(var l=4294967295,n=0;n<c.length;n++)l=l>>>8^uf[(l^c.charCodeAt(n))&255];return((l^-1)>>>0).toString(36)},Jf=function(){return function(a){var b=Me(G.location.href),c=b.search.replace("?",""),d=He(c,"_gl",!0)||"";a.query=If(d)||{};var e=Ke(b,"fragment").match(Ff("_gl"));a.fragment=If(e&&e[3]||"")||{}}},Kf=function(a){var b=Jf(),c=xf();c.data||(c.data={query:{},fragment:{}},b(c.data));var d={},e=c.data;e&&(La(d,e.query),a&&La(d,e.fragment));return d},If=
function(a){var b;b=void 0===b?3:b;try{if(a){var c;a:{for(var d=a,e=0;3>e;++e){var f=Bf.exec(d);if(f){c=f;break a}d=decodeURIComponent(d)}c=void 0}var h=c;if(h&&"1"===h[1]){var k=h[3],l;a:{for(var n=h[2],m=0;m<b;++m)if(n===Gf(k,m)){l=!0;break a}l=!1}if(l){for(var r={},t=k?k.split("*"):[],q=0;q<t.length;q+=2)r[t[q]]=tf(t[q+1]);return r}}}}catch(u){}};
function Lf(a,b,c,d){function e(m){var r=m,t=Ff(a).exec(r),q=r;if(t){var u=t[2],w=t[4];q=t[1];w&&(q=q+u+w)}m=q;var v=m.charAt(m.length-1);m&&"&"!==v&&(m+="&");return m+n}d=void 0===d?!1:d;var f=Ef.exec(c);if(!f)return"";var h=f[1],k=f[2]||"",l=f[3]||"",n=a+"="+b;d?l="#"+e(l.substring(1)):k="?"+e(k.substring(1));return""+h+k+l}
function Mf(a,b){var c="FORM"===(a.tagName||"").toUpperCase(),d=Af(b,1,c),e=Af(b,2,c),f=Af(b,3,c);if(Ma(d)){var h=Hf(d);c?Nf("_gl",h,a):Of("_gl",h,a,!1)}if(!c&&Ma(e)){var k=Hf(e);Of("_gl",k,a,!0)}for(var l in f)if(f.hasOwnProperty(l))a:{var n=l,m=f[l],r=a;if(r.tagName){if("a"===r.tagName.toLowerCase()){Of(n,m,r,void 0);break a}if("form"===r.tagName.toLowerCase()){Nf(n,m,r);break a}}"string"==typeof r&&Lf(n,m,r,void 0)}}
function Of(a,b,c,d){if(c.href){var e=Lf(a,b,c.href,void 0===d?!1:d);sc.test(e)&&(c.href=e)}}
function Nf(a,b,c){if(c&&c.action){var d=(c.method||"").toLowerCase();if("get"===d){for(var e=c.childNodes||[],f=!1,h=0;h<e.length;h++){var k=e[h];if(k.name===a){k.setAttribute("value",b);f=!0;break}}if(!f){var l=H.createElement("input");l.setAttribute("type","hidden");l.setAttribute("name",a);l.setAttribute("value",b);c.appendChild(l)}}else if("post"===d){var n=Lf(a,b,c.action);sc.test(n)&&(c.action=n)}}}
var vf=function(a){try{var b;a:{for(var c=a,d=100;c&&0<d;){if(c.href&&c.nodeName.match(/^a(?:rea)?$/i)){b=c;break a}c=c.parentNode;d--}b=null}var e=b;if(e){var f=e.protocol;"http:"!==f&&"https:"!==f||Mf(e,e.hostname)}}catch(h){}},wf=function(a){try{if(a.action){var b=Ke(Me(a.action),"host");Mf(a,b)}}catch(c){}},Pf=function(a,b,c,d){yf();zf(a,b,"fragment"===c?2:1,!!d,!1)},Qf=function(a,b){yf();zf(a,[Je(G.location,"host",!0)],b,!0,!0)},Rf=function(){var a=H.location.hostname,b=Cf.exec(H.referrer);if(!b)return!1;
var c=b[2],d=b[1],e="";if(c){var f=c.split("/"),h=f[1];e="s"===h?decodeURIComponent(f[2]):decodeURIComponent(h)}else if(d){if(0===d.indexOf("xn--"))return!1;e=d.replace(/-/g,".").replace(/\.\./g,"-")}var k=a.replace(Df,""),l=e.replace(Df,""),n;if(!(n=k===l)){var m="."+l;n=k.substring(k.length-m.length,k.length)===m}return n},Sf=function(a,b){return!1===a?!1:a||b||Rf()};var Tf=/^\w+$/,Uf=/^[\w-]+$/,Vf=/^~?[\w-]+$/,Wf={aw:"_aw",dc:"_dc",gf:"_gf",ha:"_ha",gp:"_gp"},Xf=function(){if(!Yc("gtag_cs_api")||!hd())return!0;var a=fd("ad_storage");return null==a?!0:!!a},Yf=function(a,b){gd("ad_storage")?Xf()?a():kd(a,"ad_storage"):b?lc("TAGGING",3):jd(function(){Yf(a,!0)},["ad_storage"])},ag=function(a){var b=[];if(!H.cookie)return b;var c=Qe(a,H.cookie,void 0,"ad_storage");if(!c||0==c.length)return b;for(var d=0;d<c.length;d++){var e=Zf(c[d]);e&&-1===ya(b,e)&&b.push(e)}return $f(b)};
function bg(a){return a&&"string"==typeof a&&a.match(Tf)?a:"_gcl"}
var dg=function(){var a=Me(G.location.href),b=Ke(a,"query",!1,void 0,"gclid"),c=Ke(a,"query",!1,void 0,"gclsrc"),d=Ke(a,"query",!1,void 0,"dclid");if(!b||!c){var e=a.hash.replace("#","");b=b||He(e,"gclid",void 0);c=c||He(e,"gclsrc",void 0)}return cg(b,c,d)},cg=function(a,b,c){var d={},e=function(f,h){d[h]||(d[h]=[]);d[h].push(f)};d.gclid=a;d.gclsrc=b;d.dclid=c;if(void 0!==a&&a.match(Uf))switch(b){case void 0:e(a,"aw");break;case "aw.ds":e(a,"aw");e(a,"dc");break;case "ds":e(a,"dc");break;case "3p.ds":Yc("gtm_3pds")&&
e(a,"dc");break;case "gf":e(a,"gf");break;case "ha":e(a,"ha");break;case "gp":e(a,"gp")}c&&e(c,"dc");return d},fg=function(a){var b=dg();Yf(function(){return eg(b,a)})};
function eg(a,b,c){function d(n,m){var r=gg(n,e);r&&(Ze(r,m,f),h=!0)}b=b||{};var e=bg(b.prefix);c=c||Ia();var f=gf(b,c,!0);f.Fa="ad_storage";var h=!1,k=Math.round(c/1E3),l=function(n){return["GCL",k,n].join(".")};a.aw&&(!0===b.fi?d("aw",l("~"+a.aw[0])):d("aw",l(a.aw[0])));a.dc&&d("dc",l(a.dc[0]));a.gf&&d("gf",l(a.gf[0]));a.ha&&d("ha",l(a.ha[0]));a.gp&&d("gp",l(a.gp[0]));return h}
var ig=function(a,b){var c=Kf(!0);Yf(function(){for(var d=bg(b.prefix),e=0;e<a.length;++e){var f=a[e];if(void 0!==Wf[f]){var h=gg(f,d),k=c[h];if(k){var l=Math.min(hg(k),Ia()),n;b:{for(var m=l,r=Qe(h,H.cookie,void 0,"ad_storage"),t=0;t<r.length;++t)if(hg(r[t])>m){n=!0;break b}n=!1}if(!n){var q=gf(b,l,!0);q.Fa="ad_storage";Ze(h,k,q)}}}}eg(cg(c.gclid,c.gclsrc),b)})},gg=function(a,b){var c=Wf[a];if(void 0!==c)return b+c},hg=function(a){var b=a.split(".");return 3!==b.length||"GCL"!==b[0]?0:1E3*(Number(b[1])||
0)};function Zf(a){var b=a.split(".");if(3==b.length&&"GCL"==b[0]&&b[1])return b[2]}
var jg=function(a,b,c,d,e){if(xa(b)){var f=bg(e),h=function(){for(var k={},l=0;l<a.length;++l){var n=gg(a[l],f);if(n){var m=Qe(n,H.cookie,void 0,"ad_storage");m.length&&(k[n]=m.sort()[m.length-1])}}return k};Yf(function(){Pf(h,b,c,d)})}},$f=function(a){return a.filter(function(b){return Vf.test(b)})},kg=function(a,b){for(var c=bg(b.prefix),d={},e=0;e<a.length;e++)Wf[a[e]]&&(d[a[e]]=Wf[a[e]]);Yf(function(){Da(d,function(f,h){var k=Qe(c+h,H.cookie,void 0,"ad_storage");if(k.length){var l=k[0],n=hg(l),
m={};m[f]=[Zf(l)];eg(m,b,n)}})})};function lg(a,b){for(var c=0;c<b.length;++c)if(a[b[c]])return!0;return!1}
var mg=function(){function a(e,f,h){h&&(e[f]=h)}var b=["aw","dc"];if(hd()){var c=dg();if(lg(c,b)){var d={};a(d,"gclid",c.gclid);a(d,"dclid",c.dclid);a(d,"gclsrc",c.gclsrc);Qf(function(){return d},3);Qf(function(){var e={};return e._up="1",e},1)}}},ng=function(){var a;if(Xf()){for(var b=[],c=H.cookie.split(";"),d=/^\s*_gac_(UA-\d+-\d+)=\s*(.+?)\s*$/,e=0;e<c.length;e++){var f=c[e].match(d);f&&b.push({sd:f[1],value:f[2]})}var h={};if(b&&b.length)for(var k=0;k<b.length;k++){var l=b[k].value.split(".");
"1"==l[0]&&3==l.length&&l[1]&&(h[b[k].sd]||(h[b[k].sd]=[]),h[b[k].sd].push({timestamp:l[1],rg:l[2]}))}a=h}else a={};return a};var og=/^\d+\.fls\.doubleclick\.net$/;function pg(a,b){gd(C.m)?od(C.m)?a():kd(a,C.m):b?E(42):qd(function(){pg(a,!0)},[C.m])}function qg(a){var b=Me(G.location.href),c=Ke(b,"host",!1);if(c&&c.match(og)){var d=Ke(b,"path").split(a+"=");if(1<d.length)return d[1].split(";")[0].split("?")[0]}}
function rg(a,b,c){if("aw"==a||"dc"==a){var d=qg("gcl"+a);if(d)return d.split(".")}var e=bg(b);if("_gcl"==e){c=void 0===c?!0:c;var f=!od(C.m)&&c,h;h=dg()[a]||[];if(0<h.length)return f?["0"]:h}var k=gg(a,e);return k?ag(k):[]}
var sg=function(a){var b=qg("gac");if(b)return!od(C.m)&&a?"0":decodeURIComponent(b);var c=ng(),d=[];Da(c,function(e,f){for(var h=[],k=0;k<f.length;k++)h.push(f[k].rg);h=$f(h);h.length&&d.push(e+":"+h.join(","))});return d.join(";")},tg=function(a,b){var c=dg().dc||[];pg(function(){nf(b);var d=jf[kf(b.prefix)],e=!1;if(d&&0<c.length){var f=K.joined_au=K.joined_au||{},h=b.prefix||"_gcl";if(!f[h])for(var k=0;k<c.length;k++){var l="https://adservice.google.com/ddm/regclk";l=l+"?gclid="+c[k]+"&auiddc="+d;Vc(l);e=f[h]=
!0}}null==a&&(a=e);if(a&&d){var n=kf(b.prefix),m=jf[n];m&&mf(n,m,b)}})};var ug=/[A-Z]+/,vg=/\s/,wg=function(a){if(g(a)&&(a=Ha(a),!vg.test(a))){var b=a.indexOf("-");if(!(0>b)){var c=a.substring(0,b);if(ug.test(c)){for(var d=a.substring(b+1).split("/"),e=0;e<d.length;e++)if(!d[e])return;return{id:a,prefix:c,containerId:c+"-"+d[0],B:d}}}}},yg=function(a){for(var b={},c=0;c<a.length;++c){var d=wg(a[c]);d&&(b[d.id]=d)}xg(b);var e=[];Da(b,function(f,h){e.push(h)});return e};
function xg(a){var b=[],c;for(c in a)if(a.hasOwnProperty(c)){var d=a[c];"AW"===d.prefix&&d.B[1]&&b.push(d.containerId)}for(var e=0;e<b.length;++e)delete a[b[e]]};var zg=function(){var a=!1;return a};var N=function(a,b,c,d){return(2===Ag()||d||"http:"!=G.location.protocol?a:b)+c},Ag=function(){var a=Mc(),b;if(1===a)a:{var c=Qd;c=c.toLowerCase();for(var d="https://"+c,e="http://"+c,f=1,h=H.getElementsByTagName("script"),k=0;k<h.length&&100>k;k++){var l=h[k].src;if(l){l=l.toLowerCase();if(0===l.indexOf(e)){b=3;break a}1===f&&0===l.indexOf(d)&&(f=2)}}b=f}else b=a;return b};
var Pg=function(a){return od(C.m)?a:a.replace(/&url=([^&#]+)/,function(b,c){var d=Ne(decodeURIComponent(c));return"&url="+encodeURIComponent(d)})};var Qg=new RegExp(/^(.*\.)?(google|youtube|blogger|withgoogle)(\.com?)?(\.[a-z]{2})?\.?$/),Rg={cl:["ecl"],customPixels:["nonGooglePixels"],ecl:["cl"],ehl:["hl"],hl:["ehl"],html:["customScripts","customPixels","nonGooglePixels","nonGoogleScripts","nonGoogleIframes"],customScripts:["html","customPixels","nonGooglePixels","nonGoogleScripts","nonGoogleIframes"],nonGooglePixels:[],nonGoogleScripts:["nonGooglePixels"],nonGoogleIframes:["nonGooglePixels"]},Sg={cl:["ecl"],customPixels:["customScripts","html"],
ecl:["cl"],ehl:["hl"],hl:["ehl"],html:["customScripts"],customScripts:["html"],nonGooglePixels:["customPixels","customScripts","html","nonGoogleScripts","nonGoogleIframes"],nonGoogleScripts:["customScripts","html"],nonGoogleIframes:["customScripts","html","nonGoogleScripts"]},Tg="google customPixels customScripts html nonGooglePixels nonGoogleScripts nonGoogleIframes".split(" ");
var Vg=function(a){var b;b||(b=xe("gtm.whitelist"));b&&E(9);
var c=b&&Na(Ga(b),Rg),d;d||(d=xe("gtm.blacklist"));d||(d=xe("tagTypeBlacklist"))&&E(3);d?E(8):d=[];Ug()&&(d=Ga(d),d.push("nonGooglePixels","nonGoogleScripts","sandboxedScripts"));0<=ya(Ga(d),"google")&&E(2);var e=d&&Na(Ga(d),Sg),f={};return function(h){var k=
h&&h[Ib.Ka];if(!k||"string"!=typeof k)return!0;k=k.replace(/^_*/,"");if(void 0!==f[k])return f[k];var l=Vd[k]||[],n=a(k,l);if(b){var m;if(m=n)a:{if(0>ya(c,k))if(l&&0<l.length)for(var r=0;r<l.length;r++){if(0>ya(c,l[r])){E(11);m=!1;break a}}else{m=!1;break a}m=!0}n=m}var t=!1;if(d){var q=0<=ya(e,k);if(q)t=q;else{var u=Ca(e,l||[]);u&&E(10);t=u}}var w=!n||t;w||!(0<=ya(l,"sandboxedScripts"))||c&&-1!==ya(c,"sandboxedScripts")||(w=Ca(e,Tg));return f[k]=w}},Ug=function(){return Qg.test(G.location&&G.location.hostname)};var Wg={active:!0,isAllowed:function(){return!0},isWhitelisted:function(){return!0}},Xg=function(a){var b=K.zones;!b&&a&&(b=K.zones=a());return b};var Yg=function(){};var Zg=!1,$g=0,ah=[];function bh(a){if(!Zg){var b=H.createEventObject,c="complete"==H.readyState,d="interactive"==H.readyState;if(!a||"readystatechange"!=a.type||c||!b&&d){Zg=!0;for(var e=0;e<ah.length;e++)I(ah[e])}ah.push=function(){for(var f=0;f<arguments.length;f++)I(arguments[f]);return 0}}}function ch(){if(!Zg&&140>$g){$g++;try{H.documentElement.doScroll("left"),bh()}catch(a){G.setTimeout(ch,50)}}}var dh=function(a){Zg?a():ah.push(a)};var eh={},fh={},gh=function(a,b,c,d){if(!fh[a]||Od[b]||"__zone"===b)return-1;var e={};Ua(d)&&(e=p(d,e));e.id=c;e.status="timeout";return fh[a].tags.push(e)-1},hh=function(a,b,c,d){if(fh[a]){var e=fh[a].tags[b];e&&(e.status=c,e.executionTime=d)}};function ih(a){for(var b=eh[a]||[],c=0;c<b.length;c++)b[c]();eh[a]={push:function(d){d(Md.C,fh[a])}}}
var lh=function(a,b,c){fh[a]={tags:[]};ua(b)&&jh(a,b);c&&G.setTimeout(function(){return ih(a)},Number(c));return kh(a)},jh=function(a,b){eh[a]=eh[a]||[];eh[a].push(Ka(function(){return I(function(){b(Md.C,fh[a])})}))};function kh(a){var b=0,c=0,d=!1;return{add:function(){c++;return Ka(function(){b++;d&&b>=c&&ih(a)})},Rf:function(){d=!0;b>=c&&ih(a)}}};var mh=function(){function a(d){return!va(d)||0>d?0:d}if(!K._li&&G.performance&&G.performance.timing){var b=G.performance.timing.navigationStart,c=va(ye.get("gtm.start"))?ye.get("gtm.start"):0;K._li={cst:a(c-b),cbt:a(Sd-b)}}};var qh={},rh=function(){return G.GoogleAnalyticsObject&&G[G.GoogleAnalyticsObject]},sh=!1;
var th=function(a){G.GoogleAnalyticsObject||(G.GoogleAnalyticsObject=a||"ga");var b=G.GoogleAnalyticsObject;if(G[b])G.hasOwnProperty(b)||E(12);else{var c=function(){c.q=c.q||[];c.q.push(arguments)};c.l=Number(new Date);G[b]=c}mh();return G[b]},uh=function(a,b,c,d){b=String(b).replace(/\s+/g,"").split(",");var e=rh();e(a+"require","linker");e(a+"linker:autoLink",b,c,d)};
var wh=function(a){},vh=function(){return G.GoogleAnalyticsObject||"ga"};
var xh=function(a,b){return function(){var c=rh(),d=c&&c.getByName&&c.getByName(a);if(d){var e=d.get("sendHitTask");d.set("sendHitTask",function(f){var h=f.get("hitPayload"),k=f.get("hitCallback"),l=0>h.indexOf("&tid="+b);l&&(f.set("hitPayload",h.replace(/&tid=UA-[0-9]+-[0-9]+/,"&tid="+b),!0),f.set("hitCallback",void 0,!0));e(f);l&&(f.set("hitPayload",h,!0),f.set("hitCallback",k,!0),f.set("_x_19",void 0,!0),e(f))})}}};function Ch(a,b,c,d){var e=wb[a],f=Dh(a,b,c,d);if(!f)return null;var h=Eb(e[Ib.$d],c,[]);if(h&&h.length){var k=h[0];f=Ch(k.index,{F:f,D:1===k.ye?b.terminate:f,terminate:b.terminate},c,d)}return f}
function Dh(a,b,c,d){function e(){if(f[Ib.tf])k();else{var v=Fb(f,c,[]),y=gh(c.id,String(f[Ib.Ka]),Number(f[Ib.be]),v[Ib.uf]),x=!1;v.vtp_gtmOnSuccess=function(){if(!x){x=!0;var z=Ia()-B;qe(c.id,wb[a],"5");hh(c.id,y,"success",z);h()}};v.vtp_gtmOnFailure=function(){if(!x){x=!0;var z=Ia()-B;qe(c.id,wb[a],"6");hh(c.id,y,"failure",z);k()}};v.vtp_gtmTagId=f.tag_id;
v.vtp_gtmEventId=c.id;qe(c.id,f,"1");var A=function(){var z=Ia()-B;qe(c.id,f,"7");hh(c.id,y,"exception",z);x||(x=!0,k())};var B=Ia();try{Cb(v,c)}catch(z){A(z)}}}var f=wb[a],h=b.F,k=b.D,l=b.terminate;if(c.Vc(f))return null;var n=Eb(f[Ib.ce],c,[]);if(n&&n.length){var m=n[0],r=Ch(m.index,{F:h,D:k,terminate:l},c,d);if(!r)return null;h=r;k=2===m.ye?l:r}if(f[Ib.Sd]||f[Ib.yf]){var t=f[Ib.Sd]?xb:c.sh,q=h,u=k;if(!t[a]){e=Ka(e);var w=Eh(a,t,e);h=w.F;k=w.D}return function(){t[a](q,u)}}return e}
function Eh(a,b,c){var d=[],e=[];b[a]=Fh(d,e,c);return{F:function(){b[a]=Gh;for(var f=0;f<d.length;f++)d[f]()},D:function(){b[a]=Hh;for(var f=0;f<e.length;f++)e[f]()}}}function Fh(a,b,c){return function(d,e){a.push(d);b.push(e);c()}}function Gh(a){a()}function Hh(a,b){b()};var Kh=function(a,b){for(var c=[],d=0;d<wb.length;d++)if(a.Ab[d]){var e=wb[d];var f=b.add();try{var h=Ch(d,{F:f,D:f,terminate:f},a,d);h?c.push({We:d,Re:Gb(e),ng:h}):(Ih(d,a),f())}catch(l){f()}}b.Rf();c.sort(Jh);for(var k=0;k<c.length;k++)c[k].ng();return 0<c.length};function Jh(a,b){var c,d=b.Re,e=a.Re;c=d>e?1:d<e?-1:0;var f;if(0!==c)f=c;else{var h=a.We,k=b.We;f=h>k?1:h<k?-1:0}return f}
function Ih(a,b){if(!ne)return;var c=function(d){var e=b.Vc(wb[d])?"3":"4",f=Eb(wb[d][Ib.$d],b,[]);f&&f.length&&c(f[0].index);qe(b.id,wb[d],e);var h=Eb(wb[d][Ib.ce],b,[]);h&&h.length&&c(h[0].index)};c(a);}
var Lh=!1,Mh=function(a,b,c,d,e){if("gtm.js"==b){if(Lh)return!1;Lh=!0}pe(a,b);var f=lh(a,d,e);Be(a,"event",1);Be(a,"ecommerce",1);Be(a,"gtm");var h={id:a,name:b,Vc:Vg(c),Ab:[],sh:[],Me:function(){E(6)}};h.Ab=Mb(h);
var k=Kh(h,f);"gtm.js"!==b&&"gtm.sync"!==b||wh(Md.C);if(!k)return k;for(var l=0;l<h.Ab.length;l++)if(h.Ab[l]){var n=wb[l];if(n&&!Od[String(n[Ib.Ka])])return!0}return!1};function Nh(a,b){if(a){var c=""+a;0!==c.indexOf("http://")&&0!==c.indexOf("https://")&&(c="https://"+c);"/"===c[c.length-1]&&(c=c.substring(0,c.length-1));return Me(""+c+b).href}}function Oh(a,b){return Ph()?Nh(a,b):void 0}
function Ph(){var a=!1;return a};var Qh=function(){this.eventModel={};this.targetConfig={};this.containerConfig={};this.h={};this.globalConfig={};this.F=function(){};this.D=function(){};this.eventId=void 0},Rh=function(a){var b=new Qh;b.eventModel=a;return b},Sh=function(a,b){a.targetConfig=b;return a},Th=function(a,b){a.containerConfig=b;return a},Uh=function(a,b){a.h=b;return a},Vh=function(a,b){a.globalConfig=b;return a},Wh=function(a,b){a.F=b;return a},Xh=function(a,b){a.D=b;return a};
Qh.prototype.getWithConfig=function(a){if(void 0!==this.eventModel[a])return this.eventModel[a];if(void 0!==this.targetConfig[a])return this.targetConfig[a];if(void 0!==this.containerConfig[a])return this.containerConfig[a];if(void 0!==this.h[a])return this.h[a];if(void 0!==this.globalConfig[a])return this.globalConfig[a]};
var Yh=function(a){function b(e){Da(e,function(f){c[f]=null})}var c={};b(a.eventModel);b(a.targetConfig);b(a.containerConfig);b(a.globalConfig);var d=[];Da(c,function(e){d.push(e)});return d};var Zh;if(3===Md.Rb.length)Zh="g";else{var $h="G";Zh=$h}
var ai={"":"n",UA:"u",AW:"a",DC:"d",G:"e",GF:"f",HA:"h",GTM:Zh,OPT:"o"},bi=function(a){var b=Md.C.split("-"),c=b[0].toUpperCase(),d=ai[c]||"i",e=a&&"GTM"===c?b[1]:"OPT"===c?b[1]:"",f;if(3===Md.Rb.length){var h="w";f="2"+h}else f="";return f+d+Md.Rb+e};var ci=function(a,b){a.addEventListener&&a.addEventListener("message",b,!1)};var di=function(){return wc("iPhone")&&!wc("iPod")&&!wc("iPad")};wc("Opera");wc("Trident")||wc("MSIE");wc("Edge");!wc("Gecko")||-1!=tc.toLowerCase().indexOf("webkit")&&!wc("Edge")||wc("Trident")||wc("MSIE")||wc("Edge");-1!=tc.toLowerCase().indexOf("webkit")&&!wc("Edge")&&wc("Mobile");wc("Macintosh");wc("Windows");wc("Linux")||wc("CrOS");var ei=ma.navigator||null;ei&&(ei.appVersion||"").indexOf("X11");wc("Android");di();wc("iPad");wc("iPod");di()||wc("iPad")||wc("iPod");tc.toLowerCase().indexOf("kaios");var fi=function(a,b){for(var c=a,d=0;50>d;++d){var e;try{e=!(!c.frames||!c.frames[b])}catch(k){e=!1}if(e)return c;var f;a:{try{var h=c.parent;if(h&&h!=c){f=h;break a}}catch(k){}f=null}if(!(c=f))break}return null};var gi=function(){};var hi=function(a){return void 0!==a.tcString&&"string"!==typeof a.tcString||void 0!==a.gdprApplies&&"boolean"!==typeof a.gdprApplies||void 0!==a.listenerId&&"number"!==typeof a.listenerId||void 0!==a.addtlConsent&&"string"!==typeof a.addtlConsent?2:a.cmpStatus&&"error"!==a.cmpStatus?0:3},ii=function(a,b){this.i=a;this.h=null;this.H={};this.ja=0;this.ma=void 0===b?500:b;this.o=null};ka(ii,gi);
var ki=function(a){return"function"===typeof a.i.__tcfapi||null!=ji(a)},ni=function(a,b){var c=setTimeout(function(){c=0;b({tcString:"tcunavailable",internalErrorState:1})},a.ma);li(a,"addEventListener",function(d){d&&mi(d)&&(d.internalErrorState=hi(d),0!=d.internalErrorState&&(d.tcString="tcunavailable"),li(a,"removeEventListener",null,d.listenerId),c&&(clearTimeout(c),c=0,b(d)))})};
ii.prototype.addEventListener=function(a){li(this,"addEventListener",function(b,c){var d=c?b:{};d.internalErrorState=hi(d);0!==d.internalErrorState&&(d.tcString="tcunavailable");a(d)})};ii.prototype.removeEventListener=function(a){a&&a.listenerId&&li(this,"removeEventListener",null,a.listenerId)};
var pi=function(a,b){if(!a.purpose||!a.vendor)return!1;var c=oi(a.vendor.consents,"755");return c&&"1"===b&&a.purposeOneTreatment&&"DE"===a.publisherCC?!0:c&&oi(a.purpose.consents,b)},oi=function(a,b){return!(!a||!a[b])},li=function(a,b,c,d){c||(c=function(){});if("function"===typeof a.i.__tcfapi){var e=a.i.__tcfapi;e(b,2,c,d)}else if(ji(a)){qi(a);var f=++a.ja;a.H[f]=c;if(a.h){var h={};a.h.postMessage((h.__tcfapiCall={command:b,version:2,callId:f,parameter:d},h),"*")}}else c({},!1)},ji=function(a){if(a.h)return a.h;
a.h=fi(a.i,"__tcfapiLocator");return a.h},qi=function(a){a.o||(a.o=function(b){try{var c,d;"string"===typeof b.data?d=JSON.parse(b.data):d=b.data;c=d.__tcfapiReturn;a.H[c.callId](c.returnValue,c.success)}catch(e){}},ci(a.i,a.o))},mi=function(a){return!1===a.gdprApplies||"error"===a.cmpStatus||"loaded"===a.cmpStatus&&("tcloaded"===a.eventStatus||"useractioncomplete"===a.eventStatus)?!0:!1};var ri={1:1,3:1,7:3,9:3,10:3};function si(a,b){if(""===a)return b;var c=Number(a);return isNaN(c)?b:c}var ti=si("",550),ui=si("",500);function vi(){var a=K.tcf||{};return K.tcf=a}
var wi=function(){var a=vi();if(!a.active){a.active=!0;var b=new ii(G,3E3),c="x";"function"===typeof G.__tcfapi?c="s":ki(b)?c="i":"function"===typeof G.__cmp?c="c":fi(G,"__cmpLocator")&&(c="d");a.Le=Ia();try{var d=!1;ni(b,function(){d=!0;a.Zc=Ia()});d&&(a.Zc=a.Le)}catch(e){c="e"}a.type=c}},Ci=function(){if(xi()?!0===G.gtag_enable_tcf_support:!1!==G.gtag_enable_tcf_support){var a=vi();if(!a.active||!a.Ea){var b=new ii(G,3E3);if("function"===typeof G.__tcfapi||ki(b)){a.active=!0;a.type="p";a.Ea={};
yi();var c=setTimeout(function(){zi(a);Ai(a);c=null},ui);try{b.addEventListener(function(d){c&&(clearTimeout(c),c=null);if(0!==d.internalErrorState)zi(a),Ai(a);else{var e;if(!1===d.gdprApplies)e=Bi(),b.removeEventListener(d);else if("tcloaded"===d.eventStatus||"useractioncomplete"===d.eventStatus||"cmpuishown"===d.eventStatus){var f={},h;for(h in ri)if(ri.hasOwnProperty(h))if("1"===h)f["1"]=mi(d)?!1===d.gdprApplies||"tcunavailable"===d.tcString?!0:pi(d,"1"):!1;else{var k=d.purpose&&d.vendor?oi(d.purpose.legitimateInterests,
h)&&oi(d.vendor.legitimateInterests,"755"):!1,l=pi(d,h),n=ri[h];1===n?f[h]=l:2===n?f[h]=k:3===n&&(f[h]=l||k)}e=f}e&&(a.tcString=d.tcString||"tcempty",a.Ea=e,Ai(a))}})}catch(d){c&&(clearTimeout(c),c=null),zi(a),Ai(a)}}}}};function zi(a){a.type="e";a.tcString="tcunavailable";a.Ea=Bi()}function yi(){var a={};md((a.ad_storage="denied",a.wait_for_update=ti,a))}var xi=function(){var a=!1;a=!0;return a};
function Bi(){var a={},b;for(b in ri)ri.hasOwnProperty(b)&&(a[b]=!0);return a}function Ai(a){var b={};nd((b.ad_storage=a.Ea["1"]?"granted":"denied",b))}var Di=function(){var a=vi();if(a.active&&void 0!==a.Zc)return Number(a.Zc-a.Le)},Ei=function(){var a=vi();return a.active&&a.Ea?a.tcString||"":""},Fi=function(a){if(!ri.hasOwnProperty(String(a)))return!0;var b=vi();return b.active&&b.Ea?!!b.Ea[String(a)]:!0};function Gi(a,b,c){function d(r){var t;K.reported_gclid||(K.reported_gclid={});t=K.reported_gclid;var q=f+(r?"gcu":"gcs");if(!t[q]){t[q]=!0;var u=[],w=function(B,z){z&&u.push(B+"="+encodeURIComponent(z))},v="https://www.google.com";if(hd()){var y=od(C.m);w("gcs",pd());r&&w("gcu","1");w("rnd",m);if((!f||h&&"aw.ds"!==h)&&od(C.m)){var x=ag("_gcl_aw");w("gclaw",x.join("."))}w("url",String(G.location).split(/[?#]/)[0]);w("dclid",Hi(b,k));!y&&b&&(v="https://pagead2.googlesyndication.com")}
w("gdpr_consent",Ei());"1"===Kf(!1)._up&&w("gtm_up","1");w("gclid",Hi(b,f));w("gclsrc",h);w("gtm",bi(!c));var A=v+"/pagead/landing?"+u.join("&");Vc(A)}}var e=dg(),f=e.gclid||"",h=e.gclsrc,k=e.dclid||"",l=!a&&(!f||h&&"aw.ds"!==h?!1:!0),n=hd();if(l||n){var m=""+bf();n?qd(function(){d();od(C.m)||kd(function(){return d(!0)},C.m)},[C.m]):d()}}function Hi(a,b){var c=a&&!od(C.m);return b&&c?"0":b}var sj=function(){var a=!0;Fi(7)&&Fi(9)&&Fi(10)||(a=!1);var b=!0;b&&!Fi(3)&&(a=!1);return a},tj=function(){var a=!0;Fi(3)||(a=!1);return a};function Nj(){var a=K;return a.gcq=a.gcq||new Oj}
var Pj=function(a,b,c){Nj().register(a,b,c)},Qj=function(a,b,c,d){Nj().push("event",[b,a],c,d)},Rj=function(a,b){Nj().push("config",[a],b)},Sj={},Tj=function(){this.status=1;this.containerConfig={};this.targetConfig={};this.i={};this.o=null;this.h=!1},Uj=function(a,b,c,d,e){this.type=a;this.o=b;this.ba=c||"";this.h=d;this.i=e},Oj=function(){this.o={};this.i={};this.h=[]},Vj=function(a,b){var c=wg(b);return a.o[c.containerId]=a.o[c.containerId]||new Tj},Wj=function(a,b,c){if(b){var d=wg(b);if(d&&1===
Vj(a,b).status){Vj(a,b).status=2;var e={};ne&&(e.timeoutId=G.setTimeout(function(){E(38);$d()},3E3));a.push("require",[e],d.containerId);Sj[d.containerId]=Ia();if(zg()){
}else{var h="/gtag/js?id="+encodeURIComponent(d.containerId)+"&l=dataLayer&cx=c",k=("http:"!=G.location.protocol?"https:":"http:")+("//www.googletagmanager.com"+h),l=Oh(c,h)||k;Lc(l)}}}},Xj=function(a,b,c,d){if(d.ba){var e=Vj(a,d.ba),f=e.o;if(f){var h=p(c),k=p(e.targetConfig[d.ba]),l=p(e.containerConfig),n=p(e.i),m=p(a.i),r=xe("gtm.uniqueEventId"),t=wg(d.ba).prefix,q=Xh(Wh(Vh(Uh(Th(Sh(Rh(h),k),l),n),m),function(){
re(r,t,"2");}),function(){re(r,t,"3");});try{re(r,t,"1");f(d.ba,b,d.o,q)}catch(u){re(r,t,"4");}}}};
Oj.prototype.register=function(a,b,c){if(3!==Vj(this,a).status){Vj(this,a).o=b;Vj(this,a).status=3;c&&(Vj(this,a).i=c);var d=wg(a),e=Sj[d.containerId];if(void 0!==e){var f=K[d.containerId].bootstrap,h=d.prefix.toUpperCase();K[d.containerId]._spx&&(h=h.toLowerCase());var k=xe("gtm.uniqueEventId"),l=h,n=Ia()-f;if(ne&&!ee[k]){k!==ae&&(Zd(),ae=k);var m=l+"."+Math.floor(f-e)+"."+Math.floor(n);je=je?je+","+m:"&cl="+m}delete Sj[d.containerId]}this.flush()}};
Oj.prototype.push=function(a,b,c,d){var e=Math.floor(Ia()/1E3);Wj(this,c,b[0][C.za]||this.i[C.za]);this.h.push(new Uj(a,e,c,b,d));d||this.flush()};
Oj.prototype.flush=function(a){for(var b=this;this.h.length;){var c=this.h[0];if(c.i)c.i=!1,this.h.push(c);else switch(c.type){case "require":if(3!==Vj(this,c.ba).status&&!a)return;ne&&G.clearTimeout(c.h[0].timeoutId);break;case "set":Da(c.h[0],function(l,n){p(Oa(l,n),b.i)});break;case "config":var d=c.h[0],e=!!d[C.kc];delete d[C.kc];var f=Vj(this,c.ba),h=wg(c.ba),k=h.containerId===h.id;e||(k?f.containerConfig={}:f.targetConfig[c.ba]={});f.h&&e||Xj(this,C.ca,d,c);f.h=!0;delete d[C.cb];k?p(d,f.containerConfig):
p(d,f.targetConfig[c.ba]);break;case "event":Xj(this,c.h[1],c.h[0],c)}this.h.shift()}};var Yj="HA GF GP G UA AW DC".split(" "),Zj=!1,ak={},bk=!1;function ck(a,b){var c={event:a};b&&(c.eventModel=p(b),b[C.Dc]&&(c.eventCallback=b[C.Dc]),b[C.Mb]&&(c.eventTimeout=b[C.Mb]));return c}var ek=function(){return Zj};
var gk={config:function(a){},event:function(a){var b=a[1];if(g(b)&&!(3<a.length)){var c;if(2<a.length){if(!Ua(a[2])&&
void 0!=a[2])return;c=a[2]}var d=ck(b,c);return d}},js:function(a){if(2==a.length&&a[1].getTime)return bk=!0,ek(),{event:"gtm.js","gtm.start":a[1].getTime()}},policy:function(){},set:function(a){var b;2==a.length&&Ua(a[1])?b=p(a[1]):3==a.length&&g(a[1])&&(b={},Ua(a[2])||xa(a[2])?b[a[1]]=p(a[2]):b[a[1]]=a[2]);if(b){
b._clear=!0;return b}},consent:function(a){if(3===a.length){var b=function(){ek()&&p(a[2],{subcommand:a[1]})};E(39);var c=a[1];"default"===c?(b(),md(a[2])):"update"===c&&(b(),nd(a[2]))}}},hk={policy:!0};var ik=function(a,b){var c=a.hide;if(c&&void 0!==c[b]&&c.end){c[b]=!1;var d=!0,e;for(e in c)if(c.hasOwnProperty(e)&&!0===c[e]){d=!1;break}d&&(c.end(),c.end=null)}},kk=function(a){var b=jk(),c=b&&b.hide;c&&c.end&&(c[a]=!0)};var lk=!1,mk=[];function nk(){if(!lk){lk=!0;for(var a=0;a<mk.length;a++)I(mk[a])}}var ok=function(a){lk?I(a):mk.push(a)};var Hk=function(a){if(Gk(a))return a;this.h=a};Hk.prototype.xg=function(){return this.h};var Gk=function(a){return!a||"object"!==Ra(a)||Ua(a)?!1:"getUntrustedUpdateValue"in a};Hk.prototype.getUntrustedUpdateValue=Hk.prototype.xg;var Ik=[],Jk=!1,Kk=function(a){return G["dataLayer"].push(a)},Lk=function(a){var b=K["dataLayer"],c=b?b.subscribers:1,d=0;return function(){++d===c&&a()}};
function Mk(a){var b=a._clear;Da(a,function(f,h){"_clear"!==f&&(b&&Ae(f,void 0),Ae(f,h))});Rd||(Rd=a["gtm.start"]);var c=a.event,d=a["gtm.uniqueEventId"];if(!c)return!1;d||(d=Wd(),a["gtm.uniqueEventId"]=d,Ae("gtm.uniqueEventId",d));var e=Nk(a);switch(c){case "gtm.init":E(19),e&&E(20)}return e}
function Nk(a){var b=a.event,c=a["gtm.uniqueEventId"],d,e=K.zones;d=e?e.checkState(Md.C,c):Wg;return d.active?Mh(c,b,d.isAllowed||d.isWhitelist,a.eventCallback,a.eventTimeout)?!0:!1:!1}
function Ok(){for(var a=!1;!Jk&&0<Ik.length;){Jk=!0;delete ue.eventModel;we();var b=Ik.shift();if(null!=b){var c=Gk(b);if(c){var d=b;b=Gk(d)?d.getUntrustedUpdateValue():void 0;for(var e=["gtm.allowlist","gtm.blocklist","gtm.whitelist","gtm.blacklist","tagTypeBlacklist"],f=0;f<e.length;f++){var h=e[f],k=xe(h,1);if(xa(k)||Ua(k))k=p(k);ve[h]=k}}try{if(ua(b))try{b.call(ye)}catch(v){}else if(xa(b)){var l=
b;if(g(l[0])){var n=l[0].split("."),m=n.pop(),r=l.slice(1),t=xe(n.join("."),2);if(void 0!==t&&null!==t)try{t[m].apply(t,r)}catch(v){}}}else{var q=b;if(q&&("[object Arguments]"==Object.prototype.toString.call(q)||Object.prototype.hasOwnProperty.call(q,"callee"))){a:{var u=b;if(u.length&&g(u[0])){var w=gk[u[0]];if(w&&(!c||!hk[u[0]])){b=w(u);break a}}b=void 0}if(!b){Jk=!1;continue}}a=Mk(b)||a}}finally{c&&we(!0)}}Jk=!1}
return!a}function Pk(){var a=Ok();try{ik(G["dataLayer"],Md.C)}catch(b){}return a}
var Rk=function(){var a=Jc("dataLayer",[]),b=Jc("google_tag_manager",{});b=b["dataLayer"]=b["dataLayer"]||{};dh(function(){b.gtmDom||(b.gtmDom=!0,a.push({event:"gtm.dom"}))});ok(function(){b.gtmLoad||(b.gtmLoad=!0,a.push({event:"gtm.load"}))});b.subscribers=(b.subscribers||0)+1;var c=a.push;a.push=function(){var e;if(0<K.SANDBOXED_JS_SEMAPHORE){e=[];for(var f=0;f<arguments.length;f++)e[f]=new Hk(arguments[f])}else e=[].slice.call(arguments,0);var h=c.apply(a,e);Ik.push.apply(Ik,e);if(300<
this.length)for(E(4);300<this.length;)this.shift();var k="boolean"!==typeof h||h;return Ok()&&k};var d=a.slice(0);Ik.push.apply(Ik,d);Qk()&&I(Pk)},Qk=function(){var a=!0;return a};var Sk={};Sk.Nb=new String("undefined");
var Tk=function(a){this.h=function(b){for(var c=[],d=0;d<a.length;d++)c.push(a[d]===Sk.Nb?b:a[d]);return c.join("")}};Tk.prototype.toString=function(){return this.h("undefined")};Tk.prototype.valueOf=Tk.prototype.toString;Sk.Ef=Tk;Sk.Gc={};Sk.gg=function(a){return new Tk(a)};var Uk={};Sk.kh=function(a,b){var c=Wd();Uk[c]=[a,b];return c};Sk.ue=function(a){var b=a?0:1;return function(c){var d=Uk[c];if(d&&"function"===typeof d[b])d[b]();Uk[c]=void 0}};Sk.Fg=function(a){for(var b=!1,c=!1,d=2;d<a.length;d++)b=
b||8===a[d],c=c||16===a[d];return b&&c};Sk.$g=function(a){if(a===Sk.Nb)return a;var b=Wd();Sk.Gc[b]=a;return'google_tag_manager["'+Md.C+'"].macro('+b+")"};Sk.Pg=function(a,b,c){a instanceof Sk.Ef&&(a=a.h(Sk.kh(b,c)),b=sa);return{Tc:a,F:b}};var Vk=function(a,b,c){function d(f,h){var k=f[h];return k}var e={event:b,"gtm.element":a,"gtm.elementClasses":d(a,"className"),"gtm.elementId":a["for"]||Rc(a,"id")||"","gtm.elementTarget":a.formTarget||d(a,"target")||""};c&&(e["gtm.triggers"]=c.join(","));e["gtm.elementUrl"]=(a.attributes&&a.attributes.formaction?a.formAction:"")||a.action||d(a,"href")||a.src||a.code||a.codebase||
"";return e},Wk=function(a){K.hasOwnProperty("autoEventsSettings")||(K.autoEventsSettings={});var b=K.autoEventsSettings;b.hasOwnProperty(a)||(b[a]={});return b[a]},Xk=function(a,b,c){Wk(a)[b]=c},Yk=function(a,b,c,d){var e=Wk(a),f=Ja(e,b,d);e[b]=c(f)},Zk=function(a,b,c){var d=Wk(a);return Ja(d,b,c)};var $k=["input","select","textarea"],al=["button","hidden","image","reset","submit"],bl=function(a){var b=a.tagName.toLowerCase();return!za($k,function(c){return c===b})||"input"===b&&za(al,function(c){return c===a.type.toLowerCase()})?!1:!0},cl=function(a){return a.form?a.form.tagName?a.form:H.getElementById(a.form):Uc(a,["form"],100)},dl=function(a,b,c){if(!a.elements)return 0;for(var d=b.getAttribute(c),e=0,f=1;e<a.elements.length;e++){var h=a.elements[e];if(bl(h)){if(h.getAttribute(c)===d)return f;
f++}}return 0};var el=!!G.MutationObserver,fl=void 0,gl=function(a){if(!fl){var b=function(){var c=H.body;if(c)if(el)(new MutationObserver(function(){for(var e=0;e<fl.length;e++)I(fl[e])})).observe(c,{childList:!0,subtree:!0});else{var d=!1;Pc(c,"DOMNodeInserted",function(){d||(d=!0,I(function(){d=!1;for(var e=0;e<fl.length;e++)I(fl[e])}))})}};fl=[];H.body?b():I(b)}fl.push(a)};
var rl=function(){var a=H.body,b=H.documentElement||a&&a.parentElement,c,d;if(H.compatMode&&"BackCompat"!==H.compatMode)c=b?b.clientHeight:0,d=b?b.clientWidth:0;else{var e=function(f,h){return f&&h?Math.min(f,h):Math.max(f,h)};E(7);c=e(b?b.clientHeight:0,a?a.clientHeight:0);d=e(b?b.clientWidth:0,a?a.clientWidth:0)}return{width:d,height:c}},sl=function(a){var b=rl(),c=b.height,d=b.width,e=a.getBoundingClientRect(),f=e.bottom-e.top,h=e.right-e.left;return f&&h?(1-Math.min((Math.max(0-e.left,0)+Math.max(e.right-
d,0))/h,1))*(1-Math.min((Math.max(0-e.top,0)+Math.max(e.bottom-c,0))/f,1)):0},tl=function(a){if(H.hidden)return!0;var b=a.getBoundingClientRect();if(b.top==b.bottom||b.left==b.right||!G.getComputedStyle)return!0;var c=G.getComputedStyle(a,null);if("hidden"===c.visibility)return!0;for(var d=a,e=c;d;){if("none"===e.display)return!0;var f=e.opacity,h=e.filter;if(h){var k=h.indexOf("opacity(");0<=k&&(h=h.substring(k+8,h.indexOf(")",k)),"%"==h.charAt(h.length-1)&&(h=h.substring(0,h.length-1)),f=Math.min(h,
f))}if(void 0!==f&&0>=f)return!0;(d=d.parentElement)&&(e=G.getComputedStyle(d,null))}return!1};var ul=[],vl=!(!G.IntersectionObserver||!G.IntersectionObserverEntry),wl=function(a,b,c){for(var d=new G.IntersectionObserver(a,{threshold:c}),e=0;e<b.length;e++)d.observe(b[e]);for(var f=0;f<ul.length;f++)if(!ul[f])return ul[f]=d,f;return ul.push(d)-1},xl=function(a,b,c){function d(k,l){var n={top:0,bottom:0,right:0,left:0,width:0,height:0},m={boundingClientRect:k.getBoundingClientRect(),
intersectionRatio:l,intersectionRect:n,isIntersecting:0<l,rootBounds:n,target:k,time:Ia()};I(function(){return a(m)})}for(var e=[],f=[],h=0;h<b.length;h++)e.push(0),f.push(-1);c.sort(function(k,l){return k-l});return function(){for(var k=0;k<b.length;k++){var l=sl(b[k]);if(l>e[k])for(;f[k]<c.length-1&&l>=c[f[k]+1];)d(b[k],l),f[k]++;else if(l<e[k])for(;0<=f[k]&&l<=c[f[k]];)d(b[k],l),f[k]--;e[k]=l}}},yl=function(a,b,c){for(var d=0;d<c.length;d++)1<c[d]?c[d]=1:0>c[d]&&(c[d]=0);if(vl){var e=!1;I(function(){e||
xl(a,b,c)()});return wl(function(f){e=!0;for(var h={Xa:0};h.Xa<f.length;h={Xa:h.Xa},h.Xa++)I(function(k){return function(){return a(f[k.Xa])}}(h))},b,c)}return G.setInterval(xl(a,b,c),1E3)},zl=function(a){vl?0<=a&&a<ul.length&&ul[a]&&(ul[a].disconnect(),ul[a]=void 0):G.clearInterval(a)};
var Al=function(a,b,c){function d(){var h=a();f+=e?(Ia()-e)*h.playbackRate/1E3:0;e=Ia()}var e=0,f=0;return{ac:function(h,k,l){var n=a(),m=n.ve,r=void 0!==l?Math.round(l):void 0!==k?Math.round(n.ve*k):Math.round(n.hg),t=void 0!==k?Math.round(100*k):0>=m?0:Math.round(r/m*100),q=H.hidden?!1:.5<=sl(c);d();var u=Vk(c,"gtm.video",[b]);u["gtm.videoProvider"]="youtube";u["gtm.videoStatus"]=h;u["gtm.videoUrl"]=n.url;u["gtm.videoTitle"]=n.title;u["gtm.videoDuration"]=Math.round(m);u["gtm.videoCurrentTime"]=
Math.round(r);u["gtm.videoElapsedTime"]=Math.round(f);u["gtm.videoPercent"]=t;u["gtm.videoVisible"]=q;Kk(u)},mh:function(){e=Ia()},Ic:function(){d()}}};var Bl=G.clearTimeout,Cl=G.setTimeout,U=function(a,b,c){if(zg()){b&&I(b)}else return Lc(a,b,c)},Dl=function(){return new Date},El=function(){return G.location.href},Fl=function(a){return Ke(Me(a),"fragment")},Gl=function(a){return Le(Me(a))},Hl=function(a,b){return xe(a,b||2)},Il=function(a,b,c){var d;b?(a.eventCallback=b,c&&(a.eventTimeout=c),d=Kk(a)):d=Kk(a);return d},Jl=function(a,b){G[a]=b},W=function(a,b,c){b&&
(void 0===G[a]||c&&!G[a])&&(G[a]=b);return G[a]},Kl=function(a,b,c){return Qe(a,b,void 0===c?!0:!!c)},Ll=function(a,b,c){return 0===Ze(a,b,c)},Ml=function(a,b){if(zg()){b&&I(b)}else Nc(a,b)},Nl=function(a){return!!Zk(a,"init",!1)},Ol=function(a){Xk(a,"init",!0)},Pl=function(a,b){var c=(void 0===b?0:b)?"www.googletagmanager.com/gtag/js":Qd;c+="?id="+encodeURIComponent(a)+"&l=dataLayer";U(N("https://","http://",c))},Ql=function(a,
b){var c=a[b];return c};var Rl=Sk.Pg;function nm(a,b){a=String(a);b=String(b);var c=a.length-b.length;return 0<=c&&a.indexOf(b,c)==c}var om=new Ba;function pm(a,b){function c(h){var k=Me(h),l=Ke(k,"protocol"),n=Ke(k,"host",!0),m=Ke(k,"port"),r=Ke(k,"path").toLowerCase().replace(/\/$/,"");if(void 0===l||"http"==l&&"80"==m||"https"==l&&"443"==m)l="web",m="default";return[l,n,m,r]}for(var d=c(String(a)),e=c(String(b)),f=0;f<d.length;f++)if(d[f]!==e[f])return!1;return!0}
function qm(a){return rm(a)?1:0}
function rm(a){var b=a.arg0,c=a.arg1;if(a.any_of&&xa(c)){for(var d=0;d<c.length;d++)if(qm({"function":a["function"],arg0:b,arg1:c[d]}))return!0;return!1}switch(a["function"]){case "_cn":return 0<=String(b).indexOf(String(c));case "_css":var e;a:{if(b){var f=["matches","webkitMatchesSelector","mozMatchesSelector","msMatchesSelector","oMatchesSelector"];try{for(var h=0;h<f.length;h++)if(b[f[h]]){e=b[f[h]](c);break a}}catch(t){}}e=!1}return e;case "_ew":return nm(b,c);case "_eq":return String(b)==String(c);
case "_ge":return Number(b)>=Number(c);case "_gt":return Number(b)>Number(c);case "_lc":var k;k=String(b).split(",");return 0<=ya(k,String(c));case "_le":return Number(b)<=Number(c);case "_lt":return Number(b)<Number(c);case "_re":var l;var n=a.ignore_case?"i":void 0;try{var m=String(c)+n,r=om.get(m);r||(r=new RegExp(c,n),om.set(m,r));l=r.test(b)}catch(t){l=!1}return l;case "_sw":return 0==String(b).indexOf(String(c));case "_um":return pm(b,c)}return!1};var sm={},tm=encodeURI,X=encodeURIComponent,um=Oc;var vm=function(a,b){if(!a)return!1;var c=Ke(Me(a),"host");if(!c)return!1;for(var d=0;b&&d<b.length;d++){var e=b[d]&&b[d].toLowerCase();if(e){var f=c.length-e.length;0<f&&"."!=e.charAt(0)&&(f--,e="."+e);if(0<=f&&c.indexOf(e,f)==f)return!0}}return!1};
var wm=function(a,b,c){for(var d={},e=!1,f=0;a&&f<a.length;f++)a[f]&&a[f].hasOwnProperty(b)&&a[f].hasOwnProperty(c)&&(d[a[f][b]]=a[f][c],e=!0);return e?d:null};sm.Gg=function(){var a=!1;return a};function Wn(){return G.gaGlobal=G.gaGlobal||{}}var Xn=function(){var a=Wn();a.hid=a.hid||Aa();return a.hid},Yn=function(a,b){var c=Wn();if(void 0==c.vid||b&&!c.from_cookie)c.vid=a,c.from_cookie=b};var ho=window,io=document,jo=function(a){var b=ho._gaUserPrefs;if(b&&b.ioo&&b.ioo()||a&&!0===ho["ga-disable-"+a])return!0;try{var c=ho.external;if(c&&c._gaUserPrefs&&"oo"==c._gaUserPrefs)return!0}catch(f){}for(var d=Oe("AMP_TOKEN",String(io.cookie),!0),e=0;e<d.length;e++)if("$OPT_OUT"==d[e])return!0;return io.getElementById("__gaOptOutExtension")?!0:!1};function mo(a){delete a.eventModel[C.cb];oo(a.eventModel)}var oo=function(a){Da(a,function(c){"_"===c.charAt(0)&&delete a[c]});var b=a[C.la]||{};Da(b,function(c){"_"===c.charAt(0)&&delete b[c]})};var ro=function(a,b,c){Qj(b,c,a)},so=function(a,b,c){Qj(b,c,a,!0)},uo=function(a,b){};
function to(a,b){}var Z={a:{}};Z.a.ctv=["google"],function(){(function(a){Z.__ctv=a;Z.__ctv.b="ctv";Z.__ctv.g=!0;Z.__ctv.priorityOverride=0})(function(){return"206"})}();

Z.a.jsm=["customScripts"],function(){(function(a){Z.__jsm=a;Z.__jsm.b="jsm";Z.__jsm.g=!0;Z.__jsm.priorityOverride=0})(function(a){if(void 0!==a.vtp_javascript){var b=a.vtp_javascript;try{var c=W("google_tag_manager");return c&&c.e&&c.e(b)}catch(d){}}})}();
Z.a.c=["google"],function(){(function(a){Z.__c=a;Z.__c.b="c";Z.__c.g=!0;Z.__c.priorityOverride=0})(function(a){return a.vtp_value})}();
Z.a.d=["google"],function(){(function(a){Z.__d=a;Z.__d.b="d";Z.__d.g=!0;Z.__d.priorityOverride=0})(function(a){var b=null,c=null,d=a.vtp_attributeName;if("CSS"==a.vtp_selectorType){var e=sd(a.vtp_elementSelector);e&&0<e.length&&(b=e[0])}else b=H.getElementById(a.vtp_elementId);b&&(d?c=Rc(b,d):c=Sc(b));return Ha(String(b&&c))})}();
Z.a.e=["google"],function(){(function(a){Z.__e=a;Z.__e.b="e";Z.__e.g=!0;Z.__e.priorityOverride=0})(function(a){return String(Ce(a.vtp_gtmEventId,"event"))})}();
Z.a.f=["google"],function(){(function(a){Z.__f=a;Z.__f.b="f";Z.__f.g=!0;Z.__f.priorityOverride=0})(function(a){var b=Hl("gtm.referrer",1)||H.referrer;return b?a.vtp_component&&"URL"!=a.vtp_component?Ke(Me(String(b)),a.vtp_component,a.vtp_stripWww,a.vtp_defaultPages,a.vtp_queryKey):Gl(String(b)):String(b)})}();
Z.a.cl=["google"],function(){function a(b){var c=b.target;if(c){var d=Vk(c,"gtm.click");Il(d)}}(function(b){Z.__cl=b;Z.__cl.b="cl";Z.__cl.g=!0;Z.__cl.priorityOverride=0})(function(b){if(!Nl("cl")){var c=W("document");Pc(c,"click",a,!0);Ol("cl")}I(b.vtp_gtmOnSuccess)})}();Z.a.k=["google"],function(){(function(a){Z.__k=a;Z.__k.b="k";Z.__k.g=!0;Z.__k.priorityOverride=0})(function(a){return Kl(a.vtp_name,Hl("gtm.cookie",1),!!a.vtp_decodeCookie)[0]})}();

Z.a.u=["google"],function(){var a=function(b){return{toString:function(){return b}}};(function(b){Z.__u=b;Z.__u.b="u";Z.__u.g=!0;Z.__u.priorityOverride=0})(function(b){var c;b.vtp_customUrlSource?c=b.vtp_customUrlSource:c=Hl("gtm.url",1);c=c||El();var d=b[a("vtp_component")];if(!d||"URL"==d)return Gl(String(c));var e=Me(String(c)),f;if("QUERY"===d)a:{var h=b[a("vtp_multiQueryKeys").toString()],k=b[a("vtp_queryKey").toString()]||"",l=b[a("vtp_ignoreEmptyQueryParam").toString()],n;h?xa(k)?n=k:n=String(k).replace(/\s+/g,
"").split(","):n=[String(k)];for(var m=0;m<n.length;m++){var r=Ke(e,"QUERY",void 0,void 0,n[m]);if(void 0!=r&&(!l||""!==r)){f=r;break a}}f=void 0}else f=Ke(e,d,"HOST"==d?b[a("vtp_stripWww")]:void 0,"PATH"==d?b[a("vtp_defaultPages")]:void 0,void 0);return f})}();
Z.a.v=["google"],function(){(function(a){Z.__v=a;Z.__v.b="v";Z.__v.g=!0;Z.__v.priorityOverride=0})(function(a){var b=a.vtp_name;if(!b||!b.replace)return!1;var c=Hl(b.replace(/\\\./g,"."),a.vtp_dataLayerVersion||1);return void 0!==c?c:a.vtp_defaultValue})}();
Z.a.ua=["google"],function(){function a(m,r){if(hd()&&!d[m]){var t=function(){var q=rh(),u="gtm"+Wd(),w=l(r),v={name:u};k(w,v,!0);q("create",m,v);q(function(){q.remove(u)})};kd(t,C.J);kd(t,C.m);d[m]=!0}}var b,c={},d={},e=function(m){qd(function(){n(m)},[C.J,C.m])},f={name:!0,clientId:!0,sampleRate:!0,siteSpeedSampleRate:!0,alwaysSendReferrer:!0,allowAnchor:!0,allowLinker:!0,cookieName:!0,cookieDomain:!0,cookieExpires:!0,cookiePath:!0,cookieUpdate:!0,cookieFlags:!0,legacyCookieDomain:!0,legacyHistoryImport:!0,
storage:!0,useAmpClientId:!0,storeGac:!0,_cd2l:!0},h={allowAnchor:!0,allowLinker:!0,alwaysSendReferrer:!0,anonymizeIp:!0,cookieUpdate:!0,exFatal:!0,forceSSL:!0,javaEnabled:!0,legacyHistoryImport:!0,nonInteraction:!0,useAmpClientId:!0,useBeacon:!0,storeGac:!0,allowAdFeatures:!0,allowAdPersonalizationSignals:!0,_cd2l:!0},k=function(m,r,t){var q=0;if(m)for(var u in m)if(m.hasOwnProperty(u)&&(t&&f[u]||!t&&void 0===f[u])){var w=h[u]?Fa(m[u]):m[u];"anonymizeIp"!=u||w||(w=void 0);r[u]=w;q++}return q},l=
function(m){var r={};m.vtp_gaSettings&&p(wm(m.vtp_gaSettings.vtp_fieldsToSet,"fieldName","value"),r);p(wm(m.vtp_fieldsToSet,"fieldName","value"),r);od(C.J)||(r.storage="none");od(C.m)||(r.allowAdFeatures=!1,r.storeGac=!1);sj()||(r.allowAdFeatures=!1);tj()||(r.allowAdPersonalizationSignals=!1);m.vtp_transportUrl&&(r._x_19=m.vtp_transportUrl);return r},n=function(m){function r(na,R){void 0!==R&&D("set",na,R)}var t={},q={},u={},w={};if(m.vtp_gaSettings){var v=m.vtp_gaSettings;p(wm(v.vtp_contentGroup,"index","group"),q);p(wm(v.vtp_dimension,"index","dimension"),u);p(wm(v.vtp_metric,"index","metric"),w);var y=p(v);y.vtp_fieldsToSet=void 0;y.vtp_contentGroup=void 0;y.vtp_dimension=void 0;y.vtp_metric=void 0;m=p(m,y)}p(wm(m.vtp_contentGroup,"index","group"),q);p(wm(m.vtp_dimension,
"index","dimension"),u);p(wm(m.vtp_metric,"index","metric"),w);var x=l(m),A=th(m.vtp_functionName);if(ua(A)){var B="",z="";m.vtp_setTrackerName&&"string"==typeof m.vtp_trackerName?""!==m.vtp_trackerName&&(z=m.vtp_trackerName,B=z+"."):(z="gtm"+Wd(),B=z+".");var D=function(na){var R=[].slice.call(arguments,0);R[0]=B+R[0];A.apply(window,R)},F=function(na,R){return void 0===R?R:na(R)},L=function(na,R){if(R)for(var nb in R)R.hasOwnProperty(nb)&&D("set",na+nb,R[nb])},O=function(){},P={name:z};k(x,P,!0);var Y=m.vtp_trackingId||t.trackingId;A("create",Y,P);D("set","&gtm",bi(!0));hd()&&(D("set","&gcs",pd()),a(Y,m));x._x_19&&(null==Ic&&delete x._x_19,
x._x_20&&!c[z]&&(c[z]=!0,A(xh(z,String(x._x_20)))));m.vtp_enableRecaptcha&&D("require","recaptcha","recaptcha.js");(function(na,R){void 0!==m[R]&&D("set",na,m[R])})("nonInteraction","vtp_nonInteraction");L("contentGroup",q);L("dimension",u);L("metric",w);var T={};k(x,T,!1)&&D("set",T);var Q;
m.vtp_enableLinkId&&D("require","linkid","linkid.js");D("set","hitCallback",function(){var na=x&&x.hitCallback;ua(na)&&na();m.vtp_gtmOnSuccess()});if("TRACK_EVENT"==m.vtp_trackType){m.vtp_enableEcommerce&&(D("require","ec","ec.js"),O());var M={hitType:"event",eventCategory:String(m.vtp_eventCategory||t.category),eventAction:String(m.vtp_eventAction||t.action),eventLabel:F(String,m.vtp_eventLabel||t.label),eventValue:F(Ea,m.vtp_eventValue||
t.value)};k(Q,M,!1);D("send",M);}else if("TRACK_SOCIAL"==m.vtp_trackType){var J={hitType:"social",socialNetwork:String(m.vtp_socialNetwork),socialAction:String(m.vtp_socialAction),socialTarget:String(m.vtp_socialActionTarget)};k(Q,J,!1);D("send",J);}else if("TRACK_TRANSACTION"==m.vtp_trackType){}else if("TRACK_TIMING"==
m.vtp_trackType){}else if("DECORATE_LINK"==m.vtp_trackType){}else if("DECORATE_FORM"==m.vtp_trackType){}else if("TRACK_DATA"==m.vtp_trackType){}else{m.vtp_enableEcommerce&&(D("require","ec","ec.js"),O());if(m.vtp_doubleClick||"DISPLAY_FEATURES"==m.vtp_advertisingFeaturesType){var ta=
"_dc_gtm_"+String(m.vtp_trackingId).replace(/[^A-Za-z0-9-]/g,"");D("require","displayfeatures",void 0,{cookieName:ta})}if("DISPLAY_FEATURES_WITH_REMARKETING_LISTS"==m.vtp_advertisingFeaturesType){var Cc="_dc_gtm_"+String(m.vtp_trackingId).replace(/[^A-Za-z0-9-]/g,"");D("require","adfeatures",{cookieName:Cc})}Q?D("send","pageview",Q):D("send","pageview");}if(!b){var ud=m.vtp_useDebugVersion?"u/analytics_debug.js":"analytics.js";m.vtp_useInternalVersion&&!m.vtp_useDebugVersion&&(ud="internal/"+ud);b=!0;var Ek=Oh(x._x_19,"/analytics.js"),Ao=N("https:","http:","//www.google-analytics.com/"+ud,x&&!!x.forceSSL);U("analytics.js"===ud&&Ek?Ek:Ao,function(){var na=rh();na&&na.loaded||m.vtp_gtmOnFailure();},m.vtp_gtmOnFailure)}}else I(m.vtp_gtmOnFailure)};
Z.__ua=e;Z.__ua.b="ua";Z.__ua.g=!0;Z.__ua.priorityOverride=0}();



Z.a.ytl=["google"],function(){function a(){var v=Math.round(1E9*Math.random())+"";return H.getElementById(v)?a():v}function b(v,y){if(!v)return!1;for(var x=0;x<t.length;x++)if(0<=v.indexOf("//"+t[x]+"/"+y))return!0;return!1}function c(v){var y=-1!==v.indexOf("?")?"&":"?";if(-1<v.indexOf("origin="))return v+y+"enablejsapi=1";if(!u){var x=W("document");u=x.location.protocol+"//"+x.location.hostname;x.location.port&&(u+=":"+x.location.port)}return v+y+"enablejsapi=1&origin="+encodeURIComponent(u)}function d(v,
y){var x=v.getAttribute("src");if(b(x,"embed/")){if(0<x.indexOf("enablejsapi=1"))return!0;if(y)return v.setAttribute("src",c(x)),!0}return!1}function e(v,y){if(!v.getAttribute("data-gtm-yt-inspected-"+y.td)&&(v.setAttribute("data-gtm-yt-inspected-"+y.td,"true"),d(v,y.Be))){v.id||(v.id=a());var x=W("YT"),A=x.get(v.id);A||(A=new x.Player(v.id));var B=h(A,y),z={},D;for(D in B)z.Ya=D,B.hasOwnProperty(z.Ya)&&A.addEventListener(z.Ya,function(F){return function(L){return B[F.Ya](L.data)}}(z)),z={Ya:z.Ya}}}
function f(v){I(function(){function y(){for(var A=x.getElementsByTagName("iframe"),B=A.length,z=0;z<B;z++)e(A[z],v)}var x=W("document");y();gl(y)})}function h(v,y){var x,A;function B(){Q=Al(function(){return{url:S,title:ea,ve:J,hg:v.getCurrentTime(),playbackRate:V}},y.td,v.getIframe());J=0;ea=S="";V=1;return z}function z(ba){switch(ba){case q.PLAYING:J=Math.round(v.getDuration());S=v.getVideoUrl();if(v.getVideoData){var ta=v.getVideoData();ea=ta?ta.title:""}V=v.getPlaybackRate();y.bg?Q.ac("start"):
Q.Ic();M=n(y.bh,y.ah,v.getDuration());return D(ba);default:return z}}function D(){wa=v.getCurrentTime();la=Dl().getTime();Q.mh();T();return F}function F(ba){var ta;switch(ba){case q.ENDED:return O(ba);case q.PAUSED:ta="pause";case q.BUFFERING:var Cc=v.getCurrentTime()-wa;ta=1<Math.abs((Dl().getTime()-la)/1E3*V-Cc)?"seek":ta||"buffering";v.getCurrentTime()&&(y.ag?Q.ac(ta):Q.Ic());Y();return L;case q.UNSTARTED:return B(ba);default:return F}}function L(ba){switch(ba){case q.ENDED:return O(ba);case q.PLAYING:return D(ba);
case q.UNSTARTED:return B(ba);default:return L}}function O(){for(;A;){var ba=x;Bl(A);ba()}y.$f&&Q.ac("complete",1);return B(q.UNSTARTED)}function P(){}function Y(){A&&(Bl(A),A=0,x=P)}function T(){if(M.length&&0!==V){var ba=-1,ta;do{ta=M[0];if(ta.Ga>v.getDuration())return;ba=(ta.Ga-v.getCurrentTime())/V;if(0>ba&&(M.shift(),0===M.length))return}while(0>ba);x=function(){A=0;x=P;0<M.length&&M[0].Ga===ta.Ga&&(M.shift(),Q.ac("progress",ta.Pe,ta.Te));T()};A=Cl(x,1E3*ba)}}var Q,M=[],J,S,ea,V,wa,la,Ta=B(q.UNSTARTED);
A=0;x=P;return{onStateChange:function(ba){Ta=Ta(ba)},onPlaybackRateChange:function(ba){wa=v.getCurrentTime();la=Dl().getTime();Q.Ic();V=ba;Y();T()}}}function k(v){for(var y=v.split(","),x=y.length,A=[],B=0;B<x;B++){var z=parseInt(y[B],10);isNaN(z)||100<z||0>z||A.push(z/100)}A.sort(function(D,F){return D-F});return A}function l(v){for(var y=v.split(","),x=y.length,A=[],B=0;B<x;B++){var z=parseInt(y[B],10);isNaN(z)||0>z||A.push(z)}A.sort(function(D,F){return D-F});return A}function n(v,y,x){var A=v.map(function(D){return{Ga:D,
Te:D,Pe:void 0}});if(!y.length)return A;var B=y.map(function(D){return{Ga:D*x,Te:void 0,Pe:D}});if(!A.length)return B;var z=A.concat(B);z.sort(function(D,F){return D.Ga-F.Ga});return z}function m(v){v.vtp_triggerStartOption?r(v):dh(function(){r(v)})}function r(v){var y=!!v.vtp_captureStart,x=!!v.vtp_captureComplete,A=!!v.vtp_capturePause,B=k(v.vtp_progressThresholdsPercent+""),z=l(v.vtp_progressThresholdsTimeInSeconds+""),D=!!v.vtp_fixMissingApi;if(y||x||A||B.length||z.length){var F={bg:y,$f:x,ag:A,
ah:B,bh:z,Be:D,td:void 0===v.vtp_uniqueTriggerId?"":v.vtp_uniqueTriggerId},L=W("YT"),O=function(){f(F)};I(v.vtp_gtmOnSuccess);if(L)L.ready&&L.ready(O);else{var P=W("onYouTubeIframeAPIReady");Jl("onYouTubeIframeAPIReady",function(){P&&P();O()});I(function(){for(var Y=W("document"),T=Y.getElementsByTagName("script"),Q=T.length,M=0;M<Q;M++){var J=T[M].getAttribute("src");if(b(J,"iframe_api")||b(J,"player_api"))return}for(var S=Y.getElementsByTagName("iframe"),ea=S.length,V=0;V<ea;V++)if(!w&&d(S[V],F.Be)){U("https://www.youtube.com/iframe_api");
w=!0;break}})}}else I(v.vtp_gtmOnSuccess)}var t=["www.youtube.com","www.youtube-nocookie.com"],q={UNSTARTED:-1,ENDED:0,PLAYING:1,PAUSED:2,BUFFERING:3,CUED:5},u,w=!1;Z.__ytl=m;Z.__ytl.b="ytl";Z.__ytl.g=!0;Z.__ytl.priorityOverride=0}();
Z.a.cid=["google"],function(){(function(a){Z.__cid=a;Z.__cid.b="cid";Z.__cid.g=!0;Z.__cid.priorityOverride=0})(function(){return Md.C})}();


Z.a.aev=["google"],function(){function a(q,u){var w=Ce(q,"gtm");if(w)return w[u]}function b(q,u,w,v){v||(v="element");var y=q+"."+u,x;if(m.hasOwnProperty(y))x=m[y];else{var A=a(q,v);if(A&&(x=w(A),m[y]=x,r.push(y),35<r.length)){var B=r.shift();delete m[B]}}return x}function c(q,u,w){var v=a(q,t[u]);return void 0!==v?v:w}function d(q,u){if(!q)return!1;var w=e(El());xa(u)||(u=String(u||"").replace(/\s+/g,"").split(","));for(var v=[w],y=0;y<u.length;y++)if(u[y]instanceof RegExp){if(u[y].test(q))return!1}else{var x=
u[y];if(0!=x.length){if(0<=e(q).indexOf(x))return!1;v.push(e(x))}}return!vm(q,v)}function e(q){n.test(q)||(q="http://"+q);return Ke(Me(q),"HOST",!0)}function f(q,u,w){switch(q){case "SUBMIT_TEXT":return b(u,"FORM."+q,h,"formSubmitElement")||w;case "LENGTH":var v=b(u,"FORM."+q,k);return void 0===v?w:v;case "INTERACTED_FIELD_ID":return l(u,"id",w);case "INTERACTED_FIELD_NAME":return l(u,"name",w);case "INTERACTED_FIELD_TYPE":return l(u,"type",w);case "INTERACTED_FIELD_POSITION":var y=a(u,"interactedFormFieldPosition");
return void 0===y?w:y;case "INTERACT_SEQUENCE_NUMBER":var x=a(u,"interactSequenceNumber");return void 0===x?w:x;default:return w}}function h(q){switch(q.tagName.toLowerCase()){case "input":return Rc(q,"value");case "button":return Sc(q);default:return null}}function k(q){if("form"===q.tagName.toLowerCase()&&q.elements){for(var u=0,w=0;w<q.elements.length;w++)bl(q.elements[w])&&u++;return u}}function l(q,u,w){var v=a(q,"interactedFormField");return v&&Rc(v,u)||w}var n=/^https?:\/\//i,m={},r=[],t={ATTRIBUTE:"elementAttribute",
CLASSES:"elementClasses",ELEMENT:"element",ID:"elementId",HISTORY_CHANGE_SOURCE:"historyChangeSource",HISTORY_NEW_STATE:"newHistoryState",HISTORY_NEW_URL_FRAGMENT:"newUrlFragment",HISTORY_OLD_STATE:"oldHistoryState",HISTORY_OLD_URL_FRAGMENT:"oldUrlFragment",TARGET:"elementTarget"};(function(q){Z.__aev=q;Z.__aev.b="aev";Z.__aev.g=!0;Z.__aev.priorityOverride=0})(function(q){var u=q.vtp_gtmEventId,w=q.vtp_defaultValue,v=q.vtp_varType;switch(v){case "TAG_NAME":var y=a(u,"element");return y&&y.tagName||
w;case "TEXT":return b(u,v,Sc)||w;case "URL":var x;a:{var A=String(a(u,"elementUrl")||w||""),B=Me(A),z=String(q.vtp_component||"URL");switch(z){case "URL":x=A;break a;case "IS_OUTBOUND":x=d(A,q.vtp_affiliatedDomains);break a;default:x=Ke(B,z,q.vtp_stripWww,q.vtp_defaultPages,q.vtp_queryKey)}}return x;case "ATTRIBUTE":var D;if(void 0===q.vtp_attribute)D=c(u,v,w);else{var F=q.vtp_attribute,L=a(u,"element");D=L&&Rc(L,F)||w||""}return D;case "MD":var O=q.vtp_mdValue,P=b(u,"MD",nl);return O&&P?ql(P,O)||
w:P||w;case "FORM":return f(String(q.vtp_component||"SUBMIT_TEXT"),u,w);default:return c(u,v,w)}})}();
Z.a.gas=["google"],function(){(function(a){Z.__gas=a;Z.__gas.b="gas";Z.__gas.g=!0;Z.__gas.priorityOverride=0})(function(a){var b=p(a),c=b;c[Ib.Ka]=null;c[Ib.lf]=null;var d=b=c;d.vtp_fieldsToSet=d.vtp_fieldsToSet||[];var e=d.vtp_cookieDomain;void 0!==e&&(d.vtp_fieldsToSet.push({fieldName:"cookieDomain",value:e}),delete d.vtp_cookieDomain);return b})}();

Z.a.hl=["google"],function(){function a(f){return f.target&&f.target.location&&f.target.location.href?f.target.location.href:El()}function b(f,h){Pc(f,"hashchange",function(k){var l=a(k);h({source:"hashchange",state:null,url:Gl(l),P:Fl(l)})})}function c(f,h){Pc(f,"popstate",function(k){var l=a(k);h({source:"popstate",state:k.state,url:Gl(l),P:Fl(l)})})}function d(f,h,k){var l=h.history,n=l[f];if(ua(n))try{l[f]=function(m,r,t){n.apply(l,[].slice.call(arguments,0));k({source:f,state:m,url:Gl(El()),
P:Fl(El())})}}catch(m){}}function e(){var f={source:null,state:W("history").state||null,url:Gl(El()),P:Fl(El())};return function(h){var k=f,l={};l[k.source]=!0;l[h.source]=!0;if(!l.popstate||!l.hashchange||k.P!=h.P){var n={event:"gtm.historyChange","gtm.historyChangeSource":h.source,"gtm.oldUrlFragment":f.P,"gtm.newUrlFragment":h.P,"gtm.oldHistoryState":f.state,"gtm.newHistoryState":h.state,"gtm.oldUrl":f.url,"gtm.newUrl":h.url};f=h;Il(n)}}}(function(f){Z.__hl=f;Z.__hl.b="hl";Z.__hl.g=!0;Z.__hl.priorityOverride=
0})(function(f){var h=W("self");if(!Nl("hl")){var k=e();b(h,k);c(h,k);d("pushState",h,k);d("replaceState",h,k);Ol("hl")}I(f.vtp_gtmOnSuccess)})}();
Z.a.fsl=[],function(){function a(){var e=W("document"),f=c(),h=HTMLFormElement.prototype.submit;Pc(e,"click",function(k){var l=k.target;if(l&&(l=Uc(l,["button","input"],100))&&("submit"==l.type||"image"==l.type)&&l.name&&Rc(l,"value")){var n;l.form?l.form.tagName?n=l.form:n=H.getElementById(l.form):n=Uc(l,["form"],100);n&&f.store(n,l)}},!1);Pc(e,"submit",function(k){var l=k.target;if(!l)return k.returnValue;var n=k.defaultPrevented||!1===k.returnValue,m=b(l)&&!n,r=f.get(l),t=!0;if(d(l,function(){if(t){var q;
r&&(q=e.createElement("input"),q.type="hidden",q.name=r.name,q.value=r.value,l.appendChild(q));h.call(l);q&&l.removeChild(q)}},n,m,r))t=!1;else return n||(k.preventDefault&&k.preventDefault(),k.returnValue=!1),!1;return k.returnValue},!1);HTMLFormElement.prototype.submit=function(){var k=this,l=b(k),n=!0;d(k,function(){n&&h.call(k)},!1,l)&&(h.call(k),n=!1)}}function b(e){var f=e.target;return f&&"_self"!==f&&"_parent"!==f&&"_top"!==f?!1:!0}function c(){var e=[],f=function(h){return za(e,function(k){return k.form===
h})};return{store:function(h,k){var l=f(h);l?l.button=k:e.push({form:h,button:k})},get:function(h){var k=f(h);return k?k.button:null}}}function d(e,f,h,k,l){var n=Zk("fsl",h?"nv.mwt":"mwt",0),m;m=h?Zk("fsl","nv.ids",[]):Zk("fsl","ids",[]);if(!m.length)return!0;var r=Vk(e,"gtm.formSubmit",m),t=e.action;t&&t.tagName&&(t=e.cloneNode(!1).action);r["gtm.elementUrl"]=t;l&&(r["gtm.formSubmitElement"]=l);if(k&&n){if(!Il(r,Lk(f),n))return!1}else Il(r,function(){},n||2E3);return!0}(function(e){Z.__fsl=e;Z.__fsl.b=
"fsl";Z.__fsl.g=!0;Z.__fsl.priorityOverride=0})(function(e){var f=e.vtp_waitForTags,h=e.vtp_checkValidation,k=Number(e.vtp_waitForTagsTimeout);if(!k||0>=k)k=2E3;var l=e.vtp_uniqueTriggerId||"0";if(f){var n=function(r){return Math.max(k,r)};Yk("fsl","mwt",n,0);h||Yk("fsl","nv.mwt",n,0)}var m=function(r){r.push(l);return r};Yk("fsl","ids",m,[]);h||Yk("fsl","nv.ids",m,[]);Nl("fsl")||(a(),Ol("fsl"));I(e.vtp_gtmOnSuccess)})}();Z.a.smm=["google"],function(){(function(a){Z.__smm=a;Z.__smm.b="smm";Z.__smm.g=!0;Z.__smm.priorityOverride=0})(function(a){var b=a.vtp_input,c=wm(a.vtp_map,"key","value")||{};return c.hasOwnProperty(b)?c[b]:a.vtp_defaultValue})}();



Z.a.paused=[],function(){(function(a){Z.__paused=a;Z.__paused.b="paused";Z.__paused.g=!0;Z.__paused.priorityOverride=0})(function(a){I(a.vtp_gtmOnFailure)})}();

Z.a.html=["customScripts"],function(){function a(d,e,f,h){return function(){try{if(0<e.length){var k=e.shift(),l=a(d,e,f,h);if("SCRIPT"==String(k.nodeName).toUpperCase()&&"text/gtmscript"==k.type){var n=H.createElement("script");n.async=!1;n.type="text/javascript";n.id=k.id;n.text=k.text||k.textContent||k.innerHTML||"";k.charset&&(n.charset=k.charset);var m=k.getAttribute("data-gtmsrc");m&&(n.src=m,Kc(n,l));d.insertBefore(n,null);m||l()}else if(k.innerHTML&&0<=k.innerHTML.toLowerCase().indexOf("<script")){for(var r=
[];k.firstChild;)r.push(k.removeChild(k.firstChild));d.insertBefore(k,null);a(k,r,l,h)()}else d.insertBefore(k,null),l()}else f()}catch(t){I(h)}}}var b=function(d,e,f){dh(function(){var h,k=K;k.postscribe||(k.postscribe=jc);h=k.postscribe;var l={done:e},n=H.createElement("div");n.style.display="none";n.style.visibility="hidden";H.body.appendChild(n);try{h(n,d,l)}catch(m){I(f)}})};var c=function(d){if(H.body){var e=
d.vtp_gtmOnFailure,f=Rl(d.vtp_html,d.vtp_gtmOnSuccess,e),h=f.Tc,k=f.F;if(d.vtp_useIframe){}else d.vtp_supportDocumentWrite?b(h,k,e):a(H.body,Tc(h),k,e)()}else Cl(function(){c(d)},
200)};Z.__html=c;Z.__html.b="html";Z.__html.g=!0;Z.__html.priorityOverride=0}();

Z.a.dbg=["google"],function(){(function(a){Z.__dbg=a;Z.__dbg.b="dbg";Z.__dbg.g=!0;Z.__dbg.priorityOverride=0})(function(){return!1})}();




Z.a.lcl=[],function(){function a(){var c=W("document"),d=0,e=function(f){var h=f.target;if(h&&3!==f.which&&!(f.Eg||f.timeStamp&&f.timeStamp===d)){d=f.timeStamp;h=Uc(h,["a","area"],100);if(!h)return f.returnValue;var k=f.defaultPrevented||!1===f.returnValue,l=Zk("lcl",k?"nv.mwt":"mwt",0),n;n=k?Zk("lcl","nv.ids",[]):Zk("lcl","ids",[]);if(n.length){var m=Vk(h,"gtm.linkClick",n);if(b(f,h,c)&&!k&&l&&h.href){var r=String(Ql(h,"rel")||""),t=!!za(r.split(" "),function(w){return"noreferrer"===w.toLowerCase()});
t&&E(36);var q=W((Ql(h,"target")||"_self").substring(1)),u=!0;if(Il(m,Lk(function(){var w;if(w=u&&q){var v;a:if(t){var y;try{y=new MouseEvent(f.type)}catch(x){if(!c.createEvent){v=!1;break a}y=c.createEvent("MouseEvents");y.initEvent(f.type,!0,!0)}y.Eg=!0;f.target.dispatchEvent(y);v=!0}else v=!1;w=!v}w&&(q.location.href=Ql(h,"href"))}),l))u=!1;else return f.preventDefault&&f.preventDefault(),f.returnValue=!1}else Il(m,function(){},l||2E3);return!0}}};Pc(c,"click",e,!1);Pc(c,"auxclick",e,!1)}function b(c,
d,e){if(2===c.which||c.ctrlKey||c.shiftKey||c.altKey||c.metaKey)return!1;var f=Ql(d,"href"),h=f.indexOf("#"),k=Ql(d,"target");if(k&&"_self"!==k&&"_parent"!==k&&"_top"!==k||0===h)return!1;if(0<h){var l=Gl(f),n=Gl(e.location);return l!==n}return!0}(function(c){Z.__lcl=c;Z.__lcl.b="lcl";Z.__lcl.g=!0;Z.__lcl.priorityOverride=0})(function(c){var d=void 0===c.vtp_waitForTags?!0:c.vtp_waitForTags,e=void 0===c.vtp_checkValidation?!0:c.vtp_checkValidation,f=Number(c.vtp_waitForTagsTimeout);if(!f||0>=f)f=2E3;
var h=c.vtp_uniqueTriggerId||"0";if(d){var k=function(n){return Math.max(f,n)};Yk("lcl","mwt",k,0);e||Yk("lcl","nv.mwt",k,0)}var l=function(n){n.push(h);return n};Yk("lcl","ids",l,[]);e||Yk("lcl","nv.ids",l,[]);Nl("lcl")||(a(),Ol("lcl"));I(c.vtp_gtmOnSuccess)})}();
Z.a.evl=["google"],function(){function a(){var f=Number(Hl("gtm.start"))||0;return Dl().getTime()-f}function b(f,h,k,l){function n(){if(!tl(f.target)){h.has(d.Qb)||h.set(d.Qb,""+a());h.has(d.Fc)||h.set(d.Fc,""+a());var r=0;h.has(d.Sb)&&(r=Number(h.get(d.Sb)));r+=100;h.set(d.Sb,""+r);if(r>=k){var t=Vk(f.target,"gtm.elementVisibility",[h.h]),q=sl(f.target);t["gtm.visibleRatio"]=Math.round(1E3*q)/10;t["gtm.visibleTime"]=k;t["gtm.visibleFirstTime"]=Number(h.get(d.Fc));t["gtm.visibleLastTime"]=Number(h.get(d.Qb));
Il(t);l()}}}if(!h.has(d.jb)&&(0==k&&n(),!h.has(d.Ma))){var m=W("self").setInterval(n,100);h.set(d.jb,m)}}function c(f){f.has(d.jb)&&(W("self").clearInterval(Number(f.get(d.jb))),f.i(d.jb))}var d={jb:"polling-id-",Fc:"first-on-screen-",Qb:"recent-on-screen-",Sb:"total-visible-time-",Ma:"has-fired-"},e=function(f,h){this.element=f;this.h=h};e.prototype.has=function(f){return!!this.element.getAttribute("data-gtm-vis-"+f+this.h)};e.prototype.get=function(f){return this.element.getAttribute("data-gtm-vis-"+
f+this.h)};e.prototype.set=function(f,h){this.element.setAttribute("data-gtm-vis-"+f+this.h,h)};e.prototype.i=function(f){this.element.removeAttribute("data-gtm-vis-"+f+this.h)};(function(f){Z.__evl=f;Z.__evl.b="evl";Z.__evl.g=!0;Z.__evl.priorityOverride=0})(function(f){function h(){var y=!1,x=null;if("CSS"===l){try{x=sd(n)}catch(F){E(46)}y=!!x&&w.length!=x.length}else if("ID"===l){var A=H.getElementById(n);A&&(x=[A],y=1!=w.length||w[0]!==A)}x||(x=[],y=0<w.length);if(y){for(var B=0;B<w.length;B++){var z=
new e(w[B],q);c(z)}w=[];for(var D=0;D<x.length;D++)w.push(x[D]);0<=v&&zl(v);0<w.length&&(v=yl(k,w,[t]))}}function k(y){var x=new e(y.target,q);y.intersectionRatio>=t?x.has(d.Ma)||b(y,x,r,"ONCE"===u?function(){for(var A=0;A<w.length;A++){var B=new e(w[A],q);B.set(d.Ma,"1");c(B)}zl(v);if(m&&fl)for(var z=0;z<fl.length;z++)fl[z]===h&&fl.splice(z,1)}:function(){x.set(d.Ma,"1");c(x)}):(c(x),"MANY_PER_ELEMENT"===u&&x.has(d.Ma)&&(x.i(d.Ma),x.i(d.Sb)),x.i(d.Qb))}var l=f.vtp_selectorType,n;"ID"===l?n=String(f.vtp_elementId):
"CSS"===l&&(n=String(f.vtp_elementSelector));var m=!!f.vtp_useDomChangeListener,r=f.vtp_useOnScreenDuration&&Number(f.vtp_onScreenDuration)||0,t=(Number(f.vtp_onScreenRatio)||50)/100,q=f.vtp_uniqueTriggerId,u=f.vtp_firingFrequency,w=[],v=-1;h();m&&gl(h);I(f.vtp_gtmOnSuccess)})}();


var vo={};vo.macro=function(a){if(Sk.Gc.hasOwnProperty(a))return Sk.Gc[a]},vo.onHtmlSuccess=Sk.ue(!0),vo.onHtmlFailure=Sk.ue(!1);vo.dataLayer=ye;vo.callback=function(a){Ud.hasOwnProperty(a)&&ua(Ud[a])&&Ud[a]();delete Ud[a]};function wo(){K[Md.C]=vo;La(Vd,Z.a);Ab=Ab||Sk;Bb=Nb}
function xo(){Xc.gtm_3pds=!0;Xc.gtag_cs_api=!0;K=G.google_tag_manager=G.google_tag_manager||{};var a=!1;Ci(),a=!0;if(K[Md.C]){var b=K.zones;b&&b.unregisterChild(Md.C);}else{for(var c=data.resource||{},d=c.macros||[],e=0;e<d.length;e++)tb.push(d[e]);for(var f=c.tags||[],h=0;h<f.length;h++)wb.push(f[h]);for(var k=c.predicates||[],l=0;l<k.length;l++)vb.push(k[l]);for(var n=c.rules||[],m=0;m<n.length;m++){for(var r=n[m],t={},q=0;q<r.length;q++)t[r[q][0]]=Array.prototype.slice.call(r[q],1);ub.push(t)}yb=Z;zb=qm;wo();Rk();Zg=!1;$g=0;if("interactive"==H.readyState&&!H.createEventObject||"complete"==H.readyState)bh();
else{Pc(H,"DOMContentLoaded",bh);Pc(H,"readystatechange",bh);if(H.createEventObject&&H.documentElement.doScroll){var u=!0;try{u=!G.frameElement}catch(x){}u&&ch()}Pc(G,"load",bh)}lk=!1;"complete"===H.readyState?nk():Pc(G,"load",nk);a:{if(!ne)break a;G.setInterval(oe,
864E5);}Sd=(new Date).getTime();}}
(function(a){
a()})(xo);

})()
