jscode <- "
shinyjs.warningBox = function(boxid) {
$('#' + boxid).parent().removeClass('box-success').addClass('box-warning');
};
shinyjs.successBox = function(boxid) {
$('#' + boxid).parent().removeClass('box-warning').addClass('box-success');
}
"

change_box <- function(box, success_condition) {
  if (success_condition) js$successBox(box)
  else js$warningBox(box)
}
