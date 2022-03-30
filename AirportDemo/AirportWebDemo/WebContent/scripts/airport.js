
$(document).ready(function () {

    $(":input[data-autocomplete]").each(function () {
        $(this).autocomplete({
             source: function (request, response) {
                 $.ajax({
                     url: 'GetMatchesServlet',
                     data: {
                         text: request.term
                     },
                     dataType: "json",
                     contentType: "application/json; charset=utf-8",
                     success: function (data) {
                         response($.map(data.d, function (item) {
                             return {
                                 value: item };
                         }));
                     }
                 });
             }
         });
    });


    $(".button").click(function () {
        $.ajax({ type: 'GET', url: 'GetDistanceServlet', dataType: "json", data: $('#CalculateForm').serialize(),
            success:
             function (response) {
                 $('#distance-km').html(response.d);
                 $('#distance-mi').html(Math.round(response.d * 0.621371192237));
             }
        });

        var describeAirport = function(airport) {
            return airport.AirportCode + ' - ' + airport.AirportName + ' airport - ' + airport.Country;
        };

        var getGmapInfo = function (airport) {
            return airport.AirportCode + ' - ' + airport.AirportName;
        };

        $.ajax({ type: 'GET',
            url: 'GetDetailsServlet',
            dataType: "json",
            data: { airport1: $("#airport1").val(), airport2: $("#airport2").val() },
            success:
             function (response) {
                 if (response.d != null){
                     $('#ap1').html(describeAirport(response.d[0]));
                     $('#ap2').html(describeAirport(response.d[1]));

                     var ap1Param = getGmapInfo(response.d[0]);
                     var ap2Param = getGmapInfo(response.d[1]);

                     $('#map').html(
                           ' <img src="https://maps.googleapis.com/maps/api/staticmap?&amp;size=288x200&amp;scale=2&amp;maptype=hybrid&amp;markers=' +
                           ap1Param + '%7c' + ap2Param + '&amp;markers=color:blue&amp;sensor=false"/>');
                 }
                 else{
                     alert("Error: Please check that the Airport Codes are valid.");
                 }
             },
            error:
             function (error) {
                 alert(error.responseText);
            }
        });
        return false;
    });
});
