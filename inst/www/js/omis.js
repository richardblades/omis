//------------------------------------------------------------------------------------------
//
//	o m i s . j s
//
//------------------------------------------------------------------------------------------



//------------------------------------------------------------------------------------------
//
// 	alertBox function
//
//------------------------------------------------------------------------------------------
function alertBox(message, target, text) {

	var cookies		= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... OMIS needs cookies to navigate - please accept.</div>';

	var database	= '<div class="alert alert-danger" role="alert">Note... The <b>Preserve</b> facility may be used to take a snapshot of the OMIS database prior to an upgrade. The <b>Restore</b> facility returns the OMIS database back to its last snapshot. Together, they maybe used to ensure that newly captured data straddles an OMIS upgrade.</div>';
	
	var directory 	= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... Only "Print" may be selected against "directory"</div>';

	var error 		= '<div class="alert alert-danger alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Error... ' + text + '</div>';

	var HTML5 		= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... This page requires a browser that supports HTML5</div>';

	var HTTP0 		= '<div class="alert alert-danger alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Error... AJAX returned an HTTP 0 error. This may occur when requesting a cross-domain resource from a server that did not include the appropriate CORS headers in the response.</div>';

	var input 		= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Input error... ' + text + '</div>';

	var missing 	= '<div class="alert alert-danger alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Error... OMIS system message could not be found</div>';

	var note 		= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... ' + text + '</div>';

	var options 	= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... Following options were selected: ' + text + '</div>';

	var RData 		= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... RData not suitable for Examine</div>';

	var server 		= '<div class="alert alert-danger alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>OpenCPU server error... ' + text + '</div>';

	var spatialWait	= '<div class="alert alert-info alert-dismissible" role="alert"> <button type="button" class="close" data-dismiss="alert" aria-label="Close"> <span aria-hidden="true">&times;</span> </button>Note... These graphics demand a great deal of calculation - your request may take up to 10 seconds</div>';
	
	switch(target) {
		case 	""				:	var target = $("#alertBox01");	break;
		case 	" "				:	var target = $("#alertBox01");	break;
		case 	"alertBox01"	:	var target = $("#alertBox01");	break;
		case 	"alertBox02"	:	var target = $("#alertBox02");	break;
		case 	"alertBox03"	:	var target = $("#alertBox03");	break;
		default					:	var target = $("#alertBox01");
	}

	switch(message) {
		case 	"cookies"		:	target.append(cookies);			break;
		case 	"database"		:	target.append(database);		break;
		case 	"directory"		:	target.append(directory);		break;
		case 	"error"			:	target.append(error);			break;
		case 	"HTML5"			:	target.append(HTML5);			break;
		case 	"HTTP0"			:	target.append(HTTP0);			break;
		case 	"input"			:	target.append(input);			break;
		case 	"note"			:	target.append(note);			break;
		case 	"options"		:	target.append(options);			break;
		case 	"RData"			:	target.append(RData);			break;
		case 	"server"		:	target.append(server);			break;
		case 	"spatialWait"	:	target.append(spatialWait);		break;
		default					:	target.append(missing);
	}
}																// alertBox function end



//------------------------------------------------------------------------------------------
//
// 	dataStack function
//
//------------------------------------------------------------------------------------------
function dataStack(geography, title) {

//------------------------------------------------------------------------------------------
//	Initialise
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Set title
//------------------------------------------------------------------------------------------
	$("#title").text(title);

//------------------------------------------------------------------------------------------
//	Adjust data item availability and value according to given geography
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Business Counts
//------------------------------------------------------------------------------------------
	if 	(geography == "nuts1") {
		$("input:radio[value='businessCounts']").parent().hide();
	}

//------------------------------------------------------------------------------------------
//	Employment
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Income Resident
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Income Workplace
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Population
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Profession
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Qualifications
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Turnover
//------------------------------------------------------------------------------------------
	if 	(geography == "nuts1") {
		$("input:radio[value='turnover']").parent().hide();
	}

//------------------------------------------------------------------------------------------
//	Provide graphic
//------------------------------------------------------------------------------------------
  	$("#plotButton").on("click", function(e) {
  		e.preventDefault();

//------------------------------------------------------------------------------------------
//  Clear away previous alert messages
//------------------------------------------------------------------------------------------
        $("#alertBox01 .alert").remove();

//------------------------------------------------------------------------------------------
// 	Determine which Options have been selected
//------------------------------------------------------------------------------------------
		var	options = [];
		$.each($("input[name='options']:checked"), function() {
        	options.push($(this).val());
        });

//------------------------------------------------------------------------------------------
// 	Disable the button during plot
//------------------------------------------------------------------------------------------
		$("#plotButton").attr("disabled", "disabled");

//------------------------------------------------------------------------------------------
// 	Perform the request
//------------------------------------------------------------------------------------------
		var req = $("#outputGraphic01")
			.rplot(
				"dataStack",
				{
				dataStack 	: $("input[name='dataStack']:checked").val(),
				geography 	: geography,
				options 	: options
				}
			);
		req.always(function() {
			$("#plotButton").removeAttr("disabled");
		});
		req.fail(function() {
			alertBox("server", null, req.responseText);
		});

	});																 // $("#plotButton")
}															   // dataStack function end



//------------------------------------------------------------------------------------------
//
// 	dataTime function
//
//------------------------------------------------------------------------------------------
function dataTime(geography, title) {

//------------------------------------------------------------------------------------------
//	Initialise
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Set title
//------------------------------------------------------------------------------------------
	$("#title").text(title);

//------------------------------------------------------------------------------------------
//	Adjust data item availability and value according to given geography
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Business Count
//------------------------------------------------------------------------------------------
	if 	(geography == "nuts1") {
		$("input:checkbox[value='numBus47s07']").parent().hide();
		$("input:checkbox[value='numBus55s07']").parent().hide();
		$("input:checkbox[value='numBus90s07']").parent().hide();
		$("#naBusinessCount").show();
	} else {
 		$("#naBusinessCount").hide();
	}

//------------------------------------------------------------------------------------------
//	Economy
//------------------------------------------------------------------------------------------
	if 			(geography == "nuts1") {
		$("input:checkbox[value='turnover47s07']").val("turnoverR");
		$("input:checkbox[value='turnover55s07']").parent().hide();
		$("input:checkbox[value='turnover90s07']").parent().hide();
	} else if 	(geography == "nuts3" || geography == "nuts4") {
		$("input:checkbox[value='aGVA']").parent().hide();
		$("input:checkbox[value='aGVAR']").parent().hide();
		$("input:checkbox[value='employCosts']").parent().hide();
		$("input:checkbox[value='employCostsR']").parent().hide();
		$("input:checkbox[value='GHDI']").parent().hide();
		$("input:checkbox[value='GVA']").parent().hide();
		$("input:checkbox[value='netCapExpnd']").parent().hide();
		$("input:checkbox[value='netCapExpndR']").parent().hide();
		$("input:checkbox[value='purchases']").parent().hide();
		$("input:checkbox[value='purchasesR']").parent().hide();
		$("input:checkbox[value='turnover']").parent().hide();
	}

//------------------------------------------------------------------------------------------
//	Employment
//------------------------------------------------------------------------------------------
	if 			(geography == "nuts1") {
		$("input:checkbox[value='femaleSelfEmp']").parent().hide();
		$("input:checkbox[value='maleSelfEmp']").parent().hide();
	} else if 	(geography == "nuts3" || geography == "nuts4") {
		$("input:checkbox[value='femaleUnemp']").parent().hide();
		$("input:checkbox[value='maleUnemp']").parent().hide();
	}
	$("input:checkbox[value='empSIC']").prop("disabled", true);

//------------------------------------------------------------------------------------------
//	Income
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Population
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Profession
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Qualifications
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Provide graphic
//------------------------------------------------------------------------------------------
  	$("#plotButton").on("click", function(e) {
  		e.preventDefault();

//------------------------------------------------------------------------------------------
//  Clear away previous alert messages
//------------------------------------------------------------------------------------------
        $("#alertBox01 .alert").remove();

//------------------------------------------------------------------------------------------
// 	Determine which variables have been checked.
//------------------------------------------------------------------------------------------
		var	data = [];
		$.each($("input[name='data']:checked"), function() {
        	data.push($(this).val());
        });
		$.each($("input[name='dataQuarter']:checked"), function() {
        	data.push($(this).val());
        });
        if (data.length > 0){
// 			alertBox("options", null, data.join(", "));
		} else {
			alertBox("input", null, "Please select data");
			return;
        }

//------------------------------------------------------------------------------------------
// 	If user has requested quarterly data then ensure at least on of these variables has
//	been selected.
//------------------------------------------------------------------------------------------
		var dataQuarter = [];
        var	optionQuarter;
		$.each($("input[name='dataQuarter']:checked"), function() {
        	dataQuarter.push($(this).val());
        });
	 	optionQuarter = $("input:checkbox[value='quarter']:checked").val();
	 	if (optionQuarter == "quarter" && dataQuarter.length == 0){
			alertBox("input", null, "Quarterly data requested. Please select from 'Employment' or 'Profession' variables.");
			return;
        }

//------------------------------------------------------------------------------------------
// 	Determine which Options have been selected
//------------------------------------------------------------------------------------------
		var	options = [];
		$.each($("input[name='options']:checked"), function() {
        	options.push($(this).val());
        });

//------------------------------------------------------------------------------------------
// 	Disable the button during plot
//------------------------------------------------------------------------------------------
		$("#plotButton").attr("disabled", "disabled");

//------------------------------------------------------------------------------------------
// 	Perform the request
//------------------------------------------------------------------------------------------
		var req = $("#outputGraphic01")
			.rplot(
				"dataTime",
				{
				data 		: data,
				geography 	: geography,
				options 	: options
				}
			);
		req.always(function() {
			$("#plotButton").removeAttr("disabled");
		});
		req.fail(function() {
			alertBox("server", null, req.responseText);
		});

	});																 // $("#plotButton")
}																// dataTime function end



//------------------------------------------------------------------------------------------
//
// 	loadSpinner function
//
//------------------------------------------------------------------------------------------
function loadSpinner() {
	var graphics = $("#outputGraphic01");

	graphics.css("background-image", "none");			// Clear out image to make ready

	var spinner = $('<span id="spinner"/>')							// Load spinner ASAP
		.appendTo(graphics)
		.attr({style : "color: gray; font-family: monospace; left: 20px; position: absolute; top: 20px; z-index: 1000; "})
		.text("loading...");
}															 // loadSpinner function end



//------------------------------------------------------------------------------------------
//
// 	loadGraphic function
//
//------------------------------------------------------------------------------------------
function loadGraphic(session) {
	var graphics 	= $("#outputGraphic01");

	var pngHeight 	= graphics.height();
	var pngWidth  	= graphics.width();
	var graphicsURL = session.getLoc()	+ "graphics/last/";

// 	setTimeout(function() { 				 	 // Emulate OpenCPU ocpu.rplot behaviour
		graphics.css("background-image", "url(" + graphicsURL +
			"png?width=" + pngWidth + "&height=" + pngHeight + ")");
		graphics.find("#spinner").remove(); 						   // Remove spinner
// 	}, 500);

	var pdf = $("<a />")
		.appendTo(graphics)
		.attr({href: graphicsURL + "pdf?width=11.69&height=8.27&paper=a4r", style: "color: gray; font-family: monospace; position: absolute; right: 10px; text-decoration: underline; top: 10px; z-index: 1000;", target: "_blank"})
		.text("pdf");

	var png = $("<a />")
		.appendTo(graphics)
		.attr({href: graphicsURL + "png?width=800&height=600", style: "color: gray; font-family: monospace; position: absolute; right: 10px; text-decoration: underline; top: 30px; z-index: 1000;", target: "_blank",})
		.text("png");

	var svg = $("<a />")
		.appendTo(graphics)
		.attr({href: graphicsURL + "svg?width=11.69&height=8.27", style: "color: gray; font-family: monospace; position: absolute; right: 10px; text-decoration: underline; top: 50px; z-index: 1000;", target: "_blank"})
		.text("svg");
}															 // loadGraphic function end



//------------------------------------------------------------------------------------------
//
// 	omis function
//
//------------------------------------------------------------------------------------------
function omis() {

//------------------------------------------------------------------------------------------
//	To animate all links starting # use the following. To stop the anchor being included in
//	URL remove the last	callback function.
//------------------------------------------------------------------------------------------
//	FIRST SOLUTION - NEGATES DROPDOWNS
//	$('#nav li a')				   // ScrollTop provides smooth scrolling for #nav links
//		.click(function(){
//			var link = $(this).attr('href');
//			alert ("link=" + link);
//			var position = $(link).offset().top;
//			alert ("position=" + position);
//
//			$('html, body')
//				.animate({scrollTop:position}, 1000, 'swing');
//	});
//	BETTER SOLUTION - LIVES WITH DROPDOWNS and SCROLLTOP at base
$('a[href^="#"]:not(a[data-toggle="dropdown"],#scrolltop,button[data-toggle="collapse"])')
		.on('click',function (e) {
		e.preventDefault();
		var target = this.hash; 											// id="#..."
		var $target = $(target);
		$('html, body').stop().animate({
			'scrollTop': $target.offset().top}, 1000, 'easeInOutExpo', function () {
				window.location.hash = target;
		});
	});

//------------------------------------------------------------------------------------------
//	Bootstrap 3.0 and newer no longer supports multiple tier dropdowns for navigation bars.
//	This snippet of jquery prevents Bootstrap from toggling the “open” classes when you go
//	past the first dropdown.
//------------------------------------------------------------------------------------------
//	FIRST SOLUTION FOUND
//	$('ul.dropdown-menu [data-toggle=dropdown]').on('click', function(event) {
//  	  	event.preventDefault();    	  // Avoid following the href location when clicking
//  	  	event.stopPropagation(); 			// Avoid having the menu close when clicking
//    	$(this).parent().addClass('open');	  	 // Re-add .open to parent sub-menu item
//    	$(this).parent().find("ul").parent().find("li.dropdown").addClass('open');
//	});
//		BETTER SOLUTION
	$('ul.dropdown-menu [data-toggle=dropdown]').on('click', function(event) {
    	event.preventDefault();    	  // Avoid following the href location when clicking
    	event.stopPropagation();	   		// Avoid having the menu close when clicking
														   // Close menu if already open
		$('ul.dropdown-menu [data-toggle=dropdown]').parent().removeClass('open');
		$(this).parent().addClass('open'); 			   		  // Open the one clicked on
		var menu = $(this).parent().find("ul");
		var menuPosn = $(menu).offset();
		if (menuPosn.left + menu.width() > $(window).width()) {
			var newPosn = -$(menu).width();
			menu.css({ left: newPosn });
		}
		else {
			var newPosn = $(this).parent().width();
			menu.css({ left: newPosn });
		}
	});



//------------------------------------------------------------------------------------------
//	SCROLLTOP
//	Standard jQuery function.
//------------------------------------------------------------------------------------------
	$('#scrolltop')
		.click(function(){
			$('html, body')
				.animate({scrollTop:0}, 1500, 'easeInOutExpo'); 			  // 'swing'
		})
}																	// omis function end



//------------------------------------------------------------------------------------------
//
// 	turnover function
//
//------------------------------------------------------------------------------------------
function turnover(geography, title) {

//------------------------------------------------------------------------------------------
//	Initialise
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Set title
//------------------------------------------------------------------------------------------
	$("#title").text(title);

//------------------------------------------------------------------------------------------
//	Adjust data item availability and value according to given geography
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Turnover
//------------------------------------------------------------------------------------------
	if 			(geography == "nuts1") {
		$("input:radio[value='turnover47s07']").val("turnoverR");
		$("input:radio[value='turnover55s07']").parent().hide();
		$("input:radio[value='turnover90s07']").parent().hide();
	} else if 	(geography == "nuts3" || geography == "nuts4") {
		$("input:radio[value='turnover']").parent().hide();
	}

//------------------------------------------------------------------------------------------
//	Business Count
//------------------------------------------------------------------------------------------
	if 	(geography == "nuts1") {
		$("input:checkbox[value='numBus47s07']").parent().hide();
		$("input:checkbox[value='numBus55s07']").parent().hide();
		$("input:checkbox[value='numBus90s07']").parent().hide();
		$("#naBusinessCount").show();
	} else {
 		$("#naBusinessCount").hide();
	}

//------------------------------------------------------------------------------------------
//	Economy
//------------------------------------------------------------------------------------------
	if 			(geography == "nuts1") {
		$("#naEconomy").hide();
	} else if 	(geography == "nuts3" || geography == "nuts4") {
		$("input:checkbox[value='aGVA']").parent().hide();
		$("input:checkbox[value='aGVAR']").parent().hide();
		$("input:checkbox[value='employCosts']").parent().hide();
		$("input:checkbox[value='employCostsR']").parent().hide();
		$("input:checkbox[value='GHDI']").parent().hide();
		$("input:checkbox[value='GVA']").parent().hide();
		$("input:checkbox[value='netCapExpnd']").parent().hide();
		$("input:checkbox[value='netCapExpndR']").parent().hide();
		$("input:checkbox[value='purchases']").parent().hide();
		$("input:checkbox[value='purchasesR']").parent().hide();
		$("#naEconomy").show();
	}

//------------------------------------------------------------------------------------------
//	Employment
//------------------------------------------------------------------------------------------
	if 			(geography == "nuts1") {
		$("input:checkbox[value='femaleSelfEmp']").parent().hide();
		$("input:checkbox[value='maleSelfEmp']").parent().hide();
	} else if 	(geography == "nuts3" || geography == "nuts4") {
		$("input:checkbox[value='femaleUnemp']").parent().hide();
		$("input:checkbox[value='maleUnemp']").parent().hide();
	}
	$("input:checkbox[value='empSIC']").prop("disabled", true);

//------------------------------------------------------------------------------------------
//	Income
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Population
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Profession
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Qualifications
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
//	Provide graphic
//------------------------------------------------------------------------------------------
  	$("#plotButton").on("click", function(e) {
  		e.preventDefault();

//------------------------------------------------------------------------------------------
//  Clear away previous alert messages
//------------------------------------------------------------------------------------------
        $("#alertBox01 .alert").remove();

//------------------------------------------------------------------------------------------
// 	Determine which turnover is required
//------------------------------------------------------------------------------------------
	 	var turnover;
	 	turnover = $("input[name='turnover']:checked").val();

//------------------------------------------------------------------------------------------
// 	Determine which independent variables have been checked.
//------------------------------------------------------------------------------------------
		var	indVars = [];
		$.each($("input[name='indVars']:checked"), function() {
        	indVars.push($(this).val());
        });
        if (indVars.length > 0){
// 			alertBox("options", null, indVars.join(", "));
		} else {
			alertBox("input", null, "Please select independent variables");
			return;
        }

//------------------------------------------------------------------------------------------
// 	Determine which Options have been selected
//------------------------------------------------------------------------------------------
		var	options = [];
		$.each($("input[name='options']:checked"), function() {
        	options.push($(this).val());
        });

//------------------------------------------------------------------------------------------
// 	Disable the button during plot
//------------------------------------------------------------------------------------------
		$("#plotButton").attr("disabled", "disabled");

//------------------------------------------------------------------------------------------
// 	Perform the request
//------------------------------------------------------------------------------------------
		var req = $("#outputGraphic01")
			.rplot(
				"turnover",
				{
				geography 	: geography,
				indVars 	: indVars,
				options 	: options,
				turnover 	: turnover
				}
			);
		req.always(function() {
			$("#plotButton").removeAttr("disabled");
		});
		req.fail(function() {
			alertBox("server", null, req.responseText);
		});

	});																 // $("#plotButton")
}																// turnover function end



//------------------------------------------------------------------------------------------
//
//	omis begins upon document ready...
//
//------------------------------------------------------------------------------------------
$(document).ready(function(){

//------------------------------------------------------------------------------------------
//	If user has entered OMIS without going through login.html then redirect them.
//------------------------------------------------------------------------------------------
	var pageName = window.location.pathname.split("/").pop();	
	if (document.cookie.indexOf("userType") == -1 &&  // indexOf returns -1 if not found
		pageName != "login.html") {
		alert("OMIS must be entered via login!"); 
		window.location.replace("login.html");
	}

//------------------------------------------------------------------------------------------
//	AJAX load generic <nav> maintained as unique, single source.
//------------------------------------------------------------------------------------------
	var ajaxNav = $('#ajaxNav');
	var userType = document.cookie.substring(9);

	ajaxNav.load('ajax.html' + ' nav', function () {
		console.log("ajaxNav loaded");
		var ajaxAdmin = $('#ajaxAdmin');
		if (userType == "admin") {
			ajaxAdmin.load('ajax.html #admin', function () {
				var admin = $('#admin');
				admin.unwrap(); 		   // Remove parent wrapper <div id="ajaxAdmin">	
				console.log("ajaxAdmin loaded");
				omis();											   // Call omis function
				console.log("omis called");
			});
		} else {
			console.log("ajaxAdmin not loaded");
			omis();												   // Call omis function
			console.log("omis called");
		}
	});
	
});													 // $(document).ready(function() end
