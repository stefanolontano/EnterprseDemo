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
import java.io.PrintWriter;

import com.microfocus.airport.AirportConstants;
import com.microfocus.airport.AirportWrapper;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * A servlet that calculates distance between two airports and stores it in a JSON object
 */
@WebServlet("/GetDistanceServlet")
public class GetDistanceServlet extends AbstractAirportServlet {
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
			int res = wrapper.GetDistance(airport1, airport2);

			PrintWriter out = response.getWriter();
			try {
				JSONObject json = new JSONObject();
				json.put(JSON_DISTANCE_KEY, res);
				out.print(json);
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}

	}

}
