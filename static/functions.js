$(document).ready(function() {
    $('.switch').click(function() {
        $(this).parent().parent().parent().next('.hide').toggle();        
        $(this).children('.pointer-up-header').toggle();        
        $(this).children('.pointer-right-header').toggle();        
    });
    $('.pointer-right').click(function() {
        $(this).parent().toggle();
        $(this).parent().parent().children('.full-link').toggle();
    })
    $('.pointer-up').click(function() {
        $(this).parent().toggle();
        $(this).parent().parent().children('.short-link').toggle();
    })
});