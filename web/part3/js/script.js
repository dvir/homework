function notification(msg) {
    $("#notification h3").html(msg);
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

function getUsersSelection() {
    var users_ids = [];
    $("input[name='user_ids[]']:checked").each(function(){ 
        users_ids.push($(this).val());
    });

    return users_ids;
}
function getContactsSelection() {
    var contacts_ids = [];
    $("input[name='contact_ids[]']:checked").each(function(){ 
        contacts_ids.push($(this).val());
    });

    return contacts_ids;
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

var firstUnhidden = 0;
var announcementsToShow = 3;
var lastUnhidden = firstUnhidden + announcementsToShow;
function rotateAnnouncements() {
    var $widget = $("#announcements");
    if (!$widget) {
        return false;
    }

    $widget.find("ul").first().fadeTo("slow", 0.2, function(){
        var $messages = $widget.find("ul > li.message");
        $messages.eq(firstUnhidden).addClass("hidden").removeClass("unhidden");
        $messages.eq(lastUnhidden).removeClass("hidden").addClass("unhidden");

        firstUnhidden = ((firstUnhidden + 1) % ($widget.find("ul").first().children().length - 1));
        lastUnhidden = ((lastUnhidden + 1) % ($widget.find("ul").first().children().length - 1));
    }).fadeTo("slow", 1);
}

$(document).ready(function(){
    $("#class_name").autocomplete({
        source: function(request, response) {
            $.ajax({
                url: "index.php",
                dataType: "json",
                data: {
                    autocomplete_classname: true,
                    q: request.term
                },
                success: response
            });
        },
        minLength: 1,
        open: function() {
            $(this).removeClass("ui-corner-all").addClass("ui-corner-top");
        },
        close: function() {
            $(this).removeClass("ui-corner-top").addClass("ui-corner-all");
        }
    });

    announcementsInterval = setInterval(function(){
        rotateAnnouncements();
    }, 4000);

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

        if ((!(/^(\d+-?)+\d+$/.test(cellphone.val()))) || cellphone.val().length < 9) {
            alert("הכנס מספר טלפון תקין המכיל ספרות ומקף בלבד");
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
                location.href = location.pathname.substring(0, location.pathname.lastIndexOf("/")+1) + '?page=classes&notification=התחברות בוצעה בהצלחה. ברוך הבא!';
            } else {
                alert("לא נמצא משתמש התואם את הפרטים שהוקדלו.");
            }
        });

        return false;
    });

    $("a.post_message").on("click", function(){
        $("form#post_message").toggle();
        clearInterval(announcementsInterval);
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

        if ((!(/^(\d+-?)+\d+$/.test(cellphone.val()))) || cellphone.val().length < 9) {
            alert("הכנס מספר טלפון תקין המכיל ספרות ומקף בלבד");
            return false;
        }

        var response = $.ajax({
            url: "index.php",
            type: "POST",
            async: false,
            data: {
                unique_email: true,
                email: email.val()
            }
        }).responseText;
        if (response == "true") {
            return true;
        } else if (response == "false") {
            alert("כתובת האימייל שבחרת כבר בשימוש. אנא בחר כתובת אחרת.");
            return false;
        } else {
            alert("אירעה שגיאה פנימית. אנא נסה שוב.");
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
        if (parseInt(hour_start.val()) == parseInt(hour_end.val())) {
            alert("שעת סיום צריכה להיות שונה משעת התחלה.");
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
                    $div.parent().load("./?page=classes&id="+id+" div#"+$div.attr("id"), function(){
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
                    $div.parent().load("./?page=classes&id="+id+" div#"+$div.attr("id"), function(){
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

    $("input#select_all_users").on("click", function(){
        var input = $(this);
        if (input.is(":checked")) {
            $("input[name='user_ids[]']").prop("checked", true);
        } else {
            $("input[name='user_ids[]']").prop("checked", false);
        }
    });
    $("input#select_all_contacts").on("click", function(){
        var input = $(this);
        if (input.is(":checked")) {
            $("input[name='contact_ids[]']").prop("checked", true);
        } else {
            $("input[name='contact_ids[]']").prop("checked", false);
        }
    });
    $("#admin_contact a.delete_selected").on("click", function(){
        var ids = getContactsSelection();
        var idsString = ids.join(",");

        if (idsString == "") {
            alert("אנא בחר רשומות למחיקה.");
            return false;
        }

        $.ajax({
            url: "index.php",
            type: "POST",
            data: {
                "delete-contacts": true,
                ids: idsString
            }
        }).done(function(data) {
            if (data == "true") {
                for (var i = 0; i < ids.length; ++i) {
                    $("#admin_contact tr.contact_"+ids[i]).remove();
                }
                notification("הרשומות שנבחרו נמחקו בהצלחה!");
            } else {
                console.log(data);
                alert("ארעה שגיאה פנימית. אנא נסה שוב.");
            }
        });
    });
    $("#admin_customers a.delete_selected").on("click", function(){
        var ids = getUsersSelection();
        var idsString = ids.join(",");

        if (idsString == "") {
            alert("אנא בחר רשומות למחיקה.");
            return false;
        }

        $.ajax({
            url: "index.php",
            type: "POST",
            data: {
                "delete-users": true,
                ids: idsString
            }
        }).done(function(data) {
            if (data == "true") {
                for (var i = 0; i < ids.length; ++i) {
                    $("#admin_customers tr.user_"+ids[i]).remove();
                }
                notification("הרשומות שנבחרו נמחקו בהצלחה!");
            } else {
                console.log(data);
                alert("ארעה שגיאה פנימית. אנא נסה שוב.");
            }
        });
    });
});
