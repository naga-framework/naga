


status ------------- controller return value from CB API ---------- code ----------------------- DOC ---------------------------------
=============================================================================================================================================
 done     {ok, Variables::proplist()}                                200  Variables will be passed into the associated Django template.

 done     {ok, Variables::proplist(), Headers::proplist()}           200  Variables will be passed into the associated Django template, 
                                                                           and Headers are HTTP headers you want to set (e.g., Content-Type).
 ----     notfound                                                   404  Invoke the 404 File Not Found handler.

 done     {redirect, Location}                                       302  Perform a 302 HTTP redirect to Location, 
                                                                           which may be a URL string or a proplist of parameters that will be
                                                                           converted to a URL using the routes system.
 done     {redirect, Location, Headers::proplist()}                  302  Perform a 302 HTTP redirect to Location and set additional 
                                                                           HTTP Headers.
 done     {moved, Location}                                          301  Perform a 301 HTTP redirect to Location, which may be a URL string 
                                                                           or a proplist of parameters that will be converted to a URL using 
                                                                           the routes system.
 done     {{moved, Location, Headers::proplist()}                    301  Perform a 301 HTTP redirect to Location and set additional 
                                                                           HTTP Headers.
 done     {action_other, OtherLocation}                              200  Execute the controller action specified by OtherLocation, 
                                                                           but without performing an HTTP redirect.
 ----     {render_other, OtherLocation}                              200  Render the view from OtherLocation, but don't actually execute 
                                                                           the associated controller action. 
 ----     {render_other, OtherLocation, Variables}                   200  Render the view from OtherLocation using Variables, 
                                                                           but don't actually execute the associated controller action.
 ----     {output, Output::iolist()}                                 200  Skip views altogether and return Output to the client.

 ----     {output, Output::iolist(), Headers::proplist()}            200  Skip views altogether and return Output to the client
                                                                           while setting additional HTTP Headers.
 ----     {stream, Generator::function(), Acc0}                      200  Stream a response to the client using HTTP chunked encoding. 
                                                                           For each chunk, the Generator function is passed
                                                                           an accumulator (initally Acc0) and should return either 
                                                                           {output, Data, Acc1} or done.
 ----     {stream, Generator::function(), Acc0, Headers::proplist()} 200  Same as above, but set additional HTTP Headers.

 ----     js                                                         200  The template will be rendered without any variables 
                                                                           and served as Content-Type: application/javascript.
 ----     {js, Variables::proplist()}                                200  Variables will be passed into the associated Django template and 
                                                                           the result will be served as Content-Type: application/javascript.
 ----     {js, Variables::proplist(), Headers::proplist()}           200  Variables will be passed into the associated Django template and 
                                                                           the result will be served as Content-Type: application/javascript.
                                                                           and Headers are HTTP headers you want to set.
 done     {json, Data::proplist()}                                   200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
 done     {json, Data::proplist(), Headers::proplist()}              200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
 ----     {jsonp, Data::proplist()}                                  200  Returns Data as a JSONP method call to the client. 
                                                                           Performs appropriate serialization if the values in Data contain a BossRecord 
                                                                           or a list of BossRecords.                                                            
 ----     {jsonp, Data::proplist(), Headers::proplist()}             200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
 ----     {jsonp, Callback::string(), Data::proplist(), Headers::proplist()} 
                                                                     200  Return Data to the client as a JSONP method call (as above) 
                                                                           while setting additional HTTP Headers.

done      {Code::integer(), Body::iolist(), Headers::proplist()}     Code Return an arbitary HTTP integer StatusCode along with a Body 
                                                                            and additional HTTP Headers.

----------- NEW ----------------------------------------------------------------------------------

done      {{json,dtl}, Data::proplist()}                             200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
                                                                           use erlydtl template to render json content.
done      {{json,dtl}, Data::proplist(), Headers::proplist()}        200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
                                                                           use erlydtl template to render json content.
done      {{json,dtl}, Data::proplist(), Headers::proplist(), Code}  200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
                                                                           use erlydtl template to render json content with 
                                                                           return an arbitary HTTP integer status Code
done      {json, Data::proplist(), Headers::proplist(), Code}        200  Return Data as a JSON object to the client. Performs appropriate 
                                                                           serialization if the values in Data contain a BossRecord or a list of BossRecords.
                                                                           use jsone encoder/decode to render json content from Data 
                                                                           return an arbitary HTTP integer status Code



===== before filter ===
