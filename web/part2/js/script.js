
function unhide(divID) {
    var item = document.getElementById(divID);
    var items = document.getElementsByClassName('unhidden'), i;
    for (i in items) {
        items[i].className = 'hidden';
    }
 
    if (item) {
        item.className = (item.className === 'hidden') ? 'unhidden' : 'hidden';
    }
}

var menuTopImageId = 0;
function changeHeaderBackground() {
    var imagesCount = 5;
    var $header = $("#header");
    menuTopImageId = ((menuTopImageId+1) % imagesCount);
    $header.fadeTo("slow", 0.2, function(){
        $header.css("background-image", "url('./img/menu_top/" + (menuTopImageId+1) + ".jpg')");
    }).fadeTo("slow", 1);
}

$(document).ready(function(){
    $("#header").on("click", function(e){
        window.location.href = "./index.html";
    });
    $("#header nav").on("click", function(e){
	e.stopPropagation();
    });

    $("#select-subject").on("change", function(){
        var $select = $(this);
        if ($select.val() == "other") {
            $("#other-subject").removeClass("hide");
        } else {
            $("#other-subject").addClass("hide");
        }
    });

    $("#contact form").on("submit", function(){
        var $form = $(this);
        
        // validate form fields
        var subject = $form.find("[name='subject']");
        var subject_other = $form.find("[name='subject_other']");
        var full_name = $form.find("[name='full_name']");
        var cellphone = $form.find("[name='cellphone']");
        var email = $form.find("[name='email']");

        if (subject.val() == "other" && subject_other.val() == "") {
            alert("הכנס את נושא הפנייה");
            return false;
        }

        if (!(/^(\d+-?)+\d+$/.test(cellphone.val()))) {
            alert("הכנס מספר טלפון המכיל ספרות ומקף בלבד");
            return false;
        }

        return false;
    });

  $("article#gallery img").on("click", function(){
        var $overlay = $(".overlay"),
            $img = $(this),
            $imgOverlay = $("img.overlay");
	$imgOverlay.attr("src", $img.attr("src"));
        $imgOverlay.css("margin-left", -1 * $imgOverlay.width() / 2);
        $imgOverlay.css("margin-top", -1 * $imgOverlay.height() / 2);
	$overlay.fadeIn();
  });
  $("div.overlay, img.overlay").on("click", function(){
        var $overlay = $(".overlay");
	$overlay.fadeOut();
  });
    setInterval(changeHeaderBackground, 7000);
});
