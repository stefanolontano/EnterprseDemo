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
package com.microfocus.airport.listeners;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import com.microfocus.airport.AirportConstants;
import com.microfocus.airport.AirportWrapper;

/**
 * Web application context listener that creates an AirportWrapper object and stores it in the 
 * application context. When the application terminates, it then deletes this object
 *
 */
public class AirportServletContextListener implements ServletContextListener {
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		
		// get path to the data file
		String root = getClass().getClassLoader().getResource("/").getPath();
		root = root.replace("WEB-INF/classes/", "data/airports.dat");
		// get rid of the leading slash
		root = root.substring(1);
		AirportWrapper wrapper = new AirportWrapper(root);
		
		ServletContext context = event.getServletContext();
		context.setAttribute(AirportConstants.AIRPORT_WRAPPER_OBJECT_PROPERTY, wrapper);
	}
	
	@Override
	public void contextDestroyed(ServletContextEvent event) {
		ServletContext context = event.getServletContext();
		AirportWrapper wrapper = (AirportWrapper)context.getAttribute(AirportConstants.AIRPORT_WRAPPER_OBJECT_PROPERTY);
		wrapper.Dispose();
		context.removeAttribute(AirportConstants.AIRPORT_WRAPPER_OBJECT_PROPERTY);
	}

}
