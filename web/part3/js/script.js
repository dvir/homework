function notification(msg) {
    $("#notification").html(msg);
    $("#notification").show();
    setTimeout(function(){
        $("#notification").fadeOut("slow");
    }, 3000);
}

function unhide(divID) {
    var item = document.getElementById(divID);
    $(".unhidden").removeClass("unhidden").addClass("hidden");
 
    if ($(item).hasClass("hidden")) {
        $(item).removeClass("hidden").addClass("unhidden");
    } else {
        $(item).removeClass("unhidden").addClass("hidden");
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
    setTimeout(function(){
        $("#notification").fadeOut("slow");
    }, 3000);
    $("#header").on("click", function(e){
        window.location.href = "./";
    });
    $("#header nav").on("click", function(e){
	e.stopPropagation();
    });

    $(".class a.edit").on("click", function(){
        var $div = $(this).parent().parent();
        $div.find("div.view").toggle();
        $div.find("div.edit").toggle();
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
        var cellphone = $form.find("[name='phone']");
        var email = $form.find("[name='email']");

        if (subject.val() == "other" && subject_other.val() == "") {
            alert("הכנס את נושא הפנייה");
            return false;
        }

        if (!(/^(\d+-?)+\d+$/.test(cellphone.val()))) {
            alert("הכנס מספר טלפון המכיל ספרות ומקף בלבד");
            return false;
        }
    });
    
    $("#login form").on("submit", function(){
        var $form = $(this);
        
        // validate form fields
        var email = $form.find("[name='email']");
        var password = $form.find("[name='password']");

        $.ajax({
                url: "index.php?page=login",
                type: "POST",
                data: {
                        login: true,
                        email: email.val(),
                        password: password.val()
                      }
        }).done(function(data) {
            if (data == "true") {
                location.href = '/?page=classes&notification=התחברות בוצעה בהצלחה. ברוך הבא!';
            } else {
                alert("לא נמצא משתמש התואם את הפרטים שהוקדלו.");
            }
        });

        return false;
    });

    $("a.post_message").on("click", function(){
        $("form#post_message").toggle();
    });

    $("#register form, #update-info form").on("submit", function(){
        var $form = $(this);
        
        // validate form fields
        var first_name = $form.find("[name='first_name']");
        var last_name = $form.find("[name='last_name']");
        var cellphone = $form.find("[name='phone']");
        var email = $form.find("[name='email']");
        var password = $form.find("[name='password']");
        var verify_password = $form.find("[name='verify_password']");

        if (first_name.val().length < 2 && first_name.val().length > 50) {
            alert("הכנס שם פרטי בין 2 ל-50 תווים.");
            return false;
        }

        if (last_name.val().length < 2 && last_name.val().length > 50) {
            alert("הכנס שם משפחה בין 2 ל-50 תווים.");
            return false;
        }

        if (password.val().length < 6) {
            alert("הכנס סיסמא עם לפחות 6 תווים.");
            return false;
        }

        if (password.val() != verify_password.val()) {
            alert("סיסמאת הוידוא לא תואמת את הסיסמא שנבחרה.");
            return false;
        }

        if (!(/^(\d+-?)+\d+$/.test(cellphone.val()))) {
            alert("הכנס מספר טלפון המכיל ספרות ומקף בלבד");
            return false;
        }
    });

    $("#create_class form").on("submit", function(){
        var $form = $(this);
        
        // validate form fields
        var name = $form.find("[name='name']");
        var description = $form.find("[name='description']");
        var hour_start = $form.find("[name='hour_start']");
        var hour_end = $form.find("[name='hour_end']");
        var capacity = $form.find("[name='capacity']");
        var price = $form.find("[name='price']");

        if (name.val().length < 2 && name.val().length > 50) {
            alert("הכנס שם חוג בין 2 ל-50 תווים.");
            return false;
        }

        if (parseInt(capacity.val()) < 1) {
            alert("כמות משתתפים חייבת להיות מספר חיובי.");
            return false;
        }

        if (parseInt(price.val()) < 0) {
            alert("מחיר חייב להיות מספר אי-שלילי.");
            return false;
        }

        if (parseInt(hour_start.val()) > parseInt(hour_end.val())) {
            alert("שעת סיום לא יכולה להתרחש לפני שעת התחלה.");
            return false;
        }
    });

    function classRegisterEvents() {
        $(".class-register").off("click");
        $(".class-unregister").off("click");
      $(".class-register").on("click", function(){
        var $div = $(this).parent().parent();
        var id = $div.attr("id").substring($div.attr("id").indexOf("-")+1); 
        $.ajax({
                url: "index.php",
                type: "POST",
                data: {
                        class_register: true,
                        id: id
                      }
        }).done(function(data) {
            if (data == "true") {
                notification("הרישום לחוג בוצע בהצלחה!");
                $div.parent().load("/?page=classes&id="+id+" div#"+$div.attr("id"), function(){
                    classRegisterEvents();
                });
            } else {
                alert("הרישום לחוג נכשל. אנא נסה שוב.");
            }
        });
      });

      $(".class-unregister").on("click", function(){
        var $div = $(this).parent().parent();
        var id = $div.attr("id").substring($div.attr("id").indexOf("-")+1); 
        $.ajax({
                url: "index.php",
                type: "POST",
                data: {
                        class_unregister: true,
                        id: id
                      }
        }).done(function(data) {
            if (data == "true") {
                notification("נמחקת מהחוג בהצלחה.");
                $div.parent().load("/?page=classes&id="+id+" div#"+$div.attr("id"), function(){
                    classRegisterEvents();
                });
            } else {
                alert("מחיקה מהחוג נכשלה. אנא נסה שוב.");
            }
        });
      });
  }
  classRegisterEvents();
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
