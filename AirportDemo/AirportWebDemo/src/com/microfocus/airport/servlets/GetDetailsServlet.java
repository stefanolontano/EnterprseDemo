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

import java.io.IOException;

import com.microfocus.airport.AirportConstants;
import com.microfocus.airport.AirportWrapper;
import com.microfocus.airport.AirportDetails;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.PrintWriter;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A servlet that looks up airport details and stores them in a JSON object
 */
@WebServlet("/GetDetailsServlet")
public class GetDetailsServlet extends AbstractAirportServlet{
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {

		String airport1 = request.getParameter(AIRPORT1_PARAMETER).toUpperCase();
		String airport2 = request.getParameter(AIRPORT2_PARAMETER).toUpperCase();
		ServletContext context = request.getSession().getServletContext();
		AirportWrapper wrapper = (AirportWrapper) context.getAttribute(AirportConstants.AIRPORT_WRAPPER_OBJECT_PROPERTY);

		synchronized (wrapper) {

			PrintWriter out = response.getWriter();

			JSONArray json = new JSONArray();
			json.put(createJsonDescription(wrapper.GetDescription(airport1)));
			json.put(createJsonDescription(wrapper.GetDescription(airport2)));

			try {
				JSONObject jsonObj = new JSONObject();
				jsonObj.put(JSON_ARRAY_KEY, json);
				out.print(jsonObj);
			} catch (JSONException e) {
				e.printStackTrace();
			}

		}
	}

	/**
	 * @param airportDetails 
	 * @return a JSON object containing airport details (airport code, name and country)
	 */
	private Object createJsonDescription(AirportDetails airportDetails) {
		if (airportDetails != null) {
			try {
				JSONObject jsonDescr = new JSONObject();
				jsonDescr.put(JSON_AIRPORT_CODE_KEY, airportDetails.getAirportCode());
				jsonDescr.put(JSON_AIRPORT_NAME_KEY, airportDetails.getAirportName());
				jsonDescr.put(JSON_COUNRY_KEY, airportDetails.getCountry());

				return jsonDescr;

			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return null;
	}
}
