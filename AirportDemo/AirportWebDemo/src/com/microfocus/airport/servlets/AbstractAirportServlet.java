/****************************************************************************
 *
 *   (C) Copyright 2014 Micro Focus or one of its affiliates.
 *
 *   The only warranties for products and services of Micro Focus and
 *   its affiliates and licensors ("Micro Focus") are set forth in the
 *   express warranty statements accompanying such products and services.
 *   Nothing herein should be construed as constituting an additional
 *   warranty.  Micro Focus shall not be liable for technical or editorial
 *   errors or omissions contained herein.  The information contained
 *   herein is subject to change without notice.
 *
 *   This sample code is supplied for demonstration purposes only
 *   on an "as is" basis and is for use at your own risk.
 * 
 ****************************************************************************/
package com.microfocus.airport.servlets;

import javax.servlet.http.HttpServlet;

/**
 * Abstract parent class for airport demo servlets
 *
 */
public abstract class AbstractAirportServlet extends HttpServlet {
	
	private static final long serialVersionUID = 1L;

	final static String AIRPORT1_PARAMETER = "airport1";
	
	final static String AIRPORT2_PARAMETER = "airport2";
	
	final static String JSON_ARRAY_KEY = "d";
	
	final static String JSON_DISTANCE_KEY = "d";
	
	final static String JSON_AIRPORT_CODE_KEY = "AirportCode";
	
	final static String JSON_AIRPORT_NAME_KEY = "AirportName";
	
	final static String JSON_COUNRY_KEY = "Country";
	
	final static String JSON_TEXT_PARAMETER = "text";
}
