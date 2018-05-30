/*
    Copyright 2018 Benjamin Vedder	benjamin@vedder.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

package rcontrolstationcomm;

import java.util.ArrayList;
import java.util.List;
import org.bridj.Pointer;
import static java.lang.System.out;

public class Utils {
	public static List<ROUTE_POINT> getRoute(int car, int mapRoute, int timeoutMs) {
		List<ROUTE_POINT> ret = new ArrayList<ROUTE_POINT>();
		
		Pointer<ROUTE_POINT> route = Pointer.allocateArray(ROUTE_POINT.class,  500);
		Pointer<Integer> len = Pointer.pointerToInts(0);
		RControlStationCommLibrary.rcsc_getRoutePoints(car, route, 
				len, 500, mapRoute, timeoutMs);
		
		int lenRes = len.get(0);
		for (int i = 0;i < lenRes;i++) {
			// Creating a new object here is necessary for some reason, otherwise
			// bridj crashes in some situation where a route is used.
			ROUTE_POINT a = new ROUTE_POINT();
			a.px(route.get(i).px());
			a.py(route.get(i).py());
			a.speed(route.get(i).speed());
			a.time(route.get(i).time());
			ret.add(a);
		}
		
		return ret;
	}
	
	// getRoutes: 
	//   Tries to read routes from RControlStation until it gets one of zero length.
	//   Returns a List of routes. 
	// ~Why is "car" relevant in the getting of routes?~
	public static List<List<ROUTE_POINT>> getRoutes(int car, int timeoutMs) { 
		int routeNum = 0; 
		
		List<ROUTE_POINT> route;  
		List<List<ROUTE_POINT>> ret = new ArrayList<List<ROUTE_POINT>>(); 
		
		// TODO: Add a query for the number of routes (req. changes to RControlStation)
		
		while ((route = getRoute(car, routeNum, timeoutMs)).size() != 0) { 
			System.out.printf("adding route\n");
			ret.add(route);
			routeNum++;
		}
		
		return ret;
	}
	
	public static boolean addRoute(int car, List<ROUTE_POINT> route, boolean replace,
			boolean mapOnly, int mapRoute, int timeoutMs) {
		Pointer<ROUTE_POINT> routePtr = Pointer.allocateArray(ROUTE_POINT.class, route.size());
				
		for (int i = 0;i < route.size();i++) {
			routePtr.set(i, route.get(i));
		}
		
		return RControlStationCommLibrary.rcsc_addRoutePoints(car,  routePtr,  route.size(), 
				replace, mapOnly, mapRoute, timeoutMs);
	}
	
	public static CAR_STATE getCarState(int car, int timeoutMs) {
		CAR_STATE st = new CAR_STATE();
		RControlStationCommLibrary.rcsc_getState(0, Pointer.pointerTo(st), timeoutMs);
		return st;
	}
	
	public static void waitUntilRouteAlmostEnded(int car) {
		CAR_STATE st = getCarState(car, 1000);
		
		while (st.ap_route_left() > 4) {
			try {
				Thread.sleep(50);
				st = getCarState(car, 1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				break;
			}
		}
	}
	
	public static void followRecoveryRoute(int car, int recoveryRoute) {
		// TODO: Make sure that the route does not cross the edge when
		// the edges are defined by a convex polygon.
		
		List<ROUTE_POINT> rec = getRoute(car, recoveryRoute, 1000);
		CAR_STATE st = getCarState(car, 1000);
		ROUTE_POINT first = new ROUTE_POINT();
		first.px(st.px());
		first.py(st.py());
		first.speed(rec.get(0).speed());
		rec.add(0, first);
				
		RControlStationCommLibrary.rcsc_setAutopilotActive(0, false, 1000);
		addRoute(car, rec, false, false, -2, 1000);
		RControlStationCommLibrary.rcsc_setAutopilotActive(0, true, 1000);
		waitPolling(car, 500);
		waitUntilRouteAlmostEnded(car);
		waitPolling(car, 3000);
		
		RControlStationCommLibrary.rcsc_setAutopilotActive(0, false, 1000);
	}
	
	public static void waitPolling(int car, int ms) {
		int timeLeft = ms;
		
		try {
			while (timeLeft > 0) {
				int sleep = 50;
				if (sleep > timeLeft) {
					sleep = timeLeft;
				}
				
				Thread.sleep(sleep);
				getCarState(car, 1000);
				timeLeft -= sleep;
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
