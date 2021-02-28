$(document).ready(function() {
    $('[data-toggle="toggle"]').change(function() {
        $(this).parents().next('.hide').toggle();
    });
    $('.pointer-down').click(function() {
        $(this).parent().toggle();
        $(this).parent().parent().children('.full-link').toggle();
    })
    $('.pointer-up').click(function() {
        $(this).parent().toggle();
        $(this).parent().parent().children('.short-link').toggle();
    })
});