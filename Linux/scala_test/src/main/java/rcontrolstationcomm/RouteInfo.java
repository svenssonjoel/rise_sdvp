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
import java.util.Random;

import static java.lang.Math.*;
import static java.lang.System.out;
  
public class RouteInfo {
	private double mXMin;
	private double mXMax;
	private double mYMin;
	private double mYMax;
	private double mLength;
	private List<ROUTE_POINT> mRoute;
	private List<List<ROUTE_POINT>> mInnerHoles;
	private Random mRandom;
	private int mLastOuterAttempts;
	private int mLastGeneratedPoints;
	
	public static class Point {
		public Point() {
			x = 0.0;
			y = 0.0;
		}
		
		public Point(double nX, double nY) {
			x = nX;
			y = nY;
		}
		
		void setTo(Point other) {
			x = other.x;
			y = other.y;
		}
		
		public double x;
		public double y;
	}

	public RouteInfo() {
		mXMin = 0.0;
		mXMax = 0.0;
		mYMin = 0.0;
		mYMax = 0.0;
		mLength = 0.0;
		mRoute = null; 
		mInnerHoles = null; 
		mRandom = new Random();
		mLastOuterAttempts = 0;
		mLastGeneratedPoints = 0;
	}

	public RouteInfo(List<ROUTE_POINT> route) {
		setRoute(route);
	}
	
	public RouteInfo(List<ROUTE_POINT> route, List<List<ROUTE_POINT>> inner) { 
		setRoute(route);
		mInnerHoles = inner; 
	}

	public void setRoute(List<ROUTE_POINT> route) {
		if (route == null || route.size() == 0) {
			return;
		}

		mRoute = route;
		mXMin = route.get(0).px();
		mXMax = route.get(0).px();
		mYMin = route.get(0).py();
		mYMax = route.get(0).py();
		mLength = 0.0;
		mRandom = new Random();
		mLastOuterAttempts = 0;
		mLastGeneratedPoints = 0;

		for (int i = 0;i < route.size();i++) {
			ROUTE_POINT p = route.get(i);
			ROUTE_POINT pp = i > 0 ? route.get(i - 1) : p;
			mLength += pointDistance(p, pp);

			if (p.px() < mXMin) {
				mXMin = p.px();
			}

			if (p.px() > mXMax) {
				mXMax = p.px();
			}

			if (p.py() < mYMin) {
				mYMin = p.py();
			}

			if (p.py() > mYMax) {
				mYMax = p.py();
			}
		}
	}

	public boolean hasRoute() {
		return mRoute == null;
	}

	public double xMin() {
		return mXMin;
	}

	public double xMax() {
		return mXMax;
	}

	public double yMin() {
		return mYMin;
	}

	public double yMax() {
		return mYMax;
	}

	public double length() {
		return mLength;
	}

	// isPointInPolygon 
	//   Static version of isPointWithinRoutePolygon that takes an extra route parameter
	public static boolean isPointInPolygon(List<ROUTE_POINT> route, double px, double py) {
		if (route == null || route.size() < 3) {
			return false;
		}

		int nVert = route.size();
		int i, j;
		boolean c = false;

		for (i = 0, j = nVert - 1;i < nVert;j = i++) {
			double vxi = route.get(i).px();
			double vyi = route.get(i).py();
			double vxj = route.get(j).px();
			double vyj = route.get(j).py();

			if (((vyi > py) != (vyj > py)) && 
					(px < (vxj-vxi) * (py-vyi) / (vyj-vyi) + vxi)) {
				c = !c;
			}
		}

		return c;

	}
	
	public boolean isPointOutsideInnerPolygons(double px, double py) {
		
		if (mInnerHoles == null) return true; 
		
		boolean isInside = false;
		for (List<ROUTE_POINT> p: mInnerHoles) {
			if (isPointInPolygon(p,px,py)) {
				isInside = true;
				System.out.printf("POINT IS IN INNER\n");
				break;
			}
		}
		return !isInside;

	}
	
	public boolean isPointWithinRoutePolygon(double px, double py) {
		return isPointInPolygon(mRoute, px, py); 
	}

	public static boolean isSegmentIntersectingPolygon(List<ROUTE_POINT>route , ROUTE_POINT p1, ROUTE_POINT p2) { 
		boolean res = false;

		if (lineIntersect(p1,p2,route.get(0), route.get(route.size()-1))) return true;
		
		for (int j = 1;j < route.size();j++) {
			ROUTE_POINT q1 = route.get(j- 1);
			ROUTE_POINT q2 = route.get(j);

			if (lineIntersect(p1, p2, q1, q2)) {
				res = true;
				break;
			}
		}

		return res;
			
	}
	public boolean isSegmentWithinRoutePolygon(ROUTE_POINT p1, ROUTE_POINT p2) {
		return !isSegmentIntersectingPolygon(mRoute, p1, p2) && isPointInPolygon(mRoute, p1.px(), p1.py());
	}
	
	public boolean isSegmentOutsideInnerPolygons(ROUTE_POINT p1, ROUTE_POINT p2) {
		
		if (mInnerHoles == null) return true;
		
		boolean isInside = false;
		for (List<ROUTE_POINT> p: mInnerHoles) {
			if (isSegmentIntersectingPolygon(p,p1,p2) || isPointInPolygon(p,p1.px(),p1.py())) {
				isInside = true;
				break;
			}
		}
		return !isInside;
				
	}
	
	// Generate a route between two points that avoid obstacles
	// TODO: Clean up 
	// TODO: change for loops (that does recursive calls) over angles into a clearer selection of, say, 3
	// TODO: Add randomization in places (for example: if to try leftmost or rightmost angle first). 
	// TODO: Threading ? (Start n instances, that each search along one path. based on seed ?) 
	public List<ROUTE_POINT> generateRouteBetween(ROUTE_POINT p1, double ang_p1, ROUTE_POINT p2, int depth) {
		if (depth >= 200) { 
			//System.out.printf("Search depth exhausted\n");
			//List<ROUTE_POINT> resRoute = new ArrayList<ROUTE_POINT>();
			//resRoute.add(p1); // TODO: REMOVE
			//return resRoute; //TODO: REMOVE!
			return null;
		}
		
		if (!(isPointWithinRoutePolygon(p1.px(), p1.py()) && isPointOutsideInnerPolygons(p1.px(), p1.py()))) { 
			if (depth == 0) { 
				// This can happen
				System.out.printf("Starting point rejected\n"); 
			} else { 
				// This "SHOULD" never happen (once the code is correct).
				System.out.printf("Generated point rejected\n");
				//List<ROUTE_POINT> resRoute = new ArrayList<ROUTE_POINT>();
				//resRoute.add(p1); // TODO: REMOVE
				//return resRoute; //TODO: REMOVE!
			}
			return null;
		}
		
		final double minDist = 0.6;
		final double maxDist = 2.0; 
		final double maxAng  = PI / 6;
		final double angIncr = PI / 8;
		
		double px1 = p1.px();
		double py1 = p1.py();
		double qx = p2.px();
		double qy = p2.py(); 
		double px2  = px1 + cos(ang_p1) * maxDist;   
		double py2  = py1 + sin(ang_p1) * maxDist;  
		double t;
		
		List<ROUTE_POINT> resRoute = new ArrayList<ROUTE_POINT>();
		
		// BASE CASE: Close enough! 
		double d = pointDistance(px1,py1,qx,qy); 
		if (d < 4.0) {
			resRoute.add(p1);
			return resRoute;
		}
		
		// BASE CASE: You are home!
		double ang = angleBetweenLines(px1, py1, px2, py2, px1, py1, qx, qy);
		if (ang < maxAng && ang > -maxAng) { 
			Point coll = new Point(0,0);
			if (!closestLineIntersection(px1,py1,qx,qy,coll)) {
				ROUTE_POINT rp = new ROUTE_POINT();
				rp.px(qx);
				rp.py(qy);
				rp.speed(3);
				resRoute.add(p1);
				resRoute.add(rp);
				System.out.printf("Home!\n");
				return resRoute;
			}
		}
				
		// check if line between p1, p2 is within turning angle. 
		if ( ang < maxAng && ang > -maxAng ) { 
			//System.out.printf("WITHIN TURN ANGLE!");
			// check if there is an intersection free line within turning angle. if there is, return route.
			Point coll = new Point();
			boolean intersect = closestLineIntersection(px1, py1, qx, qy, coll);
			
			// Abort if no intersection here. Means a fault in implementation.
			if (!intersect) { 
				System.out.printf("Should intersect! ");
				return null;
			}
			
			double collDist = pointDistance(px1,py1,coll.x,coll.y);
			
			// Give up if too close. 
			//if (collDist < minDist) return null;
					
			//Try to generate a route from a range of points generated within the turning angle
			for (double a = -maxAng; a < maxAng; a+=angIncr) { 
				// margins
				double t_x = px1 + cos(ang_p1 + a) * (maxDist + minDist); //maxDist; //(collDist - minDist);
				double t_y = py1 + sin(ang_p1 + a) * (maxDist + minDist); //maxDist; //(collDist - minDist); 
					
				double new_x = px1 + cos(ang_p1 + a) * maxDist; //maxDist; //(collDist - minDist);
				double new_y = py1 + sin(ang_p1 + a) * maxDist; //maxDist; //(collDist - minDist); 

				ROUTE_POINT new_p = new ROUTE_POINT();
				new_p.px(new_x);
				new_p.py(new_y);
				new_p.speed(3);
				Point cp = new Point();

				// only proceed to search along paths that do not enter into a polygon (with a margin)
				boolean intersects = closestLineIntersection(px1, py1,t_x, t_y,cp);
				if (!intersects || pointDistance(px1,py2,cp.x,cp.y) > (maxDist + minDist)) {	
					//System.out.printf("Generating point within turning angle\n");
					List<ROUTE_POINT> new_route = generateRouteBetween(new_p,ang_p1 + a, p2, depth +1);
					if (new_route != null) {
						// TODO use a collection where adding to front is cheap.
						//      Or add to the end and do a reverse before use. 
						new_route.add(0,p1);
						return new_route;
					}
				}
			} // FOR
		}		// Target point is not within turning angle
		
		// select maximum turn angle that would get us closer to p2 and try again
		//System.out.printf("Select MAX turn-angle closer to target\n");
		ROUTE_POINT closestPoint = new ROUTE_POINT();
		double bestAngle = 0; 
		double smallestDist = Double.MAX_VALUE;
		boolean ok = false;
		for (double a = -maxAng; a < maxAng; a+=angIncr) { 
			double t_x = px1 + cos(ang_p1 + a) * (maxDist + minDist);
			double t_y = py1 + sin(ang_p1 + a) * (maxDist + minDist); 
			
			double new_x = px1 + cos(ang_p1 + a) * maxDist;
			double new_y = py1 + sin(ang_p1 + a) * maxDist; 
			
			ROUTE_POINT new_p = new ROUTE_POINT();
			new_p.px(new_x);
			new_p.py(new_y);
			new_p.speed(3);
			Point collPoint = new Point(0,0); 
				
			// Distance to target-point.
			double dist = pointDistance(new_x, new_y, qx, qy);
			if (!closestLineIntersection(px1, py1, t_x, t_y, collPoint)) { 
				//System.out.printf("DOES THIS EVEN HAPPEN?\n");
				if (dist < smallestDist) { 
					closestPoint = new_p;
					bestAngle = ang_p1 + a;
					smallestDist = dist;
					ok = true;
				}
			}
		}
			
		if (ok) { 
			//System.out.printf("Generating point based on ok!\n");
			List<ROUTE_POINT> new_route = generateRouteBetween(closestPoint,bestAngle, p2, depth+1);
			// TODO use a collection where adding to front is cheap.
			//      Or add to the end and do a reverse before use.
			if (new_route != null) { 
				new_route.add(0,p1);
				return new_route;
			}
			//System.out.printf("Didnt work\n");
		} 
		
		// move some distance forward and try again
		double new_x = px1 + cos(ang_p1) * maxDist;
		double new_y = py1 + sin(ang_p1) * maxDist;
		Point collPoint = new Point(0,0); 
		//System.out.printf("ELSE %d ", depth);
		boolean intersects = closestLineIntersection(px1, py1, new_x, new_y, collPoint);
			
		if (!intersects || (intersects && pointDistance(px1,py1,collPoint.x,collPoint.y) > maxDist)) {
			//System.out.printf("Generating move forward point");
			ROUTE_POINT new_p = new ROUTE_POINT();
			new_p.px(new_x);
			new_p.py(new_y);
			new_p.speed(3);
			List<ROUTE_POINT> new_route = generateRouteBetween(new_p,ang_p1, p2, depth+1);
			if (new_route != null) { 
				new_route.add(0,p1);
				return new_route;
			}	
		}
		
		
		return null;
	}
	
	public List<ROUTE_POINT> generateRouteWithin(int length,
			List<ROUTE_POINT> previous, double speed) {
		
		List<ROUTE_POINT> r = new ArrayList<ROUTE_POINT>();
		List<ROUTE_POINT> rLargest = new ArrayList<ROUTE_POINT>();

		if (mRoute == null || mRoute.size() < 3) {
			return r;
		}
		
		// Generation settings
		int maxAttemptsOuter = 5000;
		int maxAttemptsInner = 50;
		double minDist = 0.6;
		double maxDist = 2.0;
		double maxAng = PI / 6;
		
		int attemptOuter = 0;
		int genPoints = 0;
		int start = 0;

		while (attemptOuter < maxAttemptsOuter) {
			r.clear();
			if (previous != null) {
				r.addAll(previous);
				start = previous.size();
			}
			
			for (int i = start;i < length + start;i++) {
				int attemptInner = 0;
				boolean ok = false;
				double px = 0.0;
				double py = 0.0;
				
				double xMin = mXMin;
				double xMax = mXMax;
				double yMin = mYMin;
				double yMax = mYMax;
				
				if (i == 1) {
					double xLast = r.get(i - 1).px();
					double yLast = r.get(i - 1).py();
					
					xMax = xLast + maxDist;
					xMin = xLast - maxDist;
					yMax = yLast + maxDist;
					yMin = yLast - maxDist;
				} else if (i > 1) {
					double xLast1 = r.get(i - 1).px();
					double yLast1 = r.get(i - 1).py();
					double xLast2 = r.get(i - 2).px();
					double yLast2 = r.get(i - 2).py();
					
					double a1 = atan2(yLast1 - yLast2, xLast1 - xLast2);
					
					double p1x = xLast1 + maxDist * cos(a1 - maxAng);
					double p1y = yLast1 + maxDist * sin(a1 - maxAng);
					double p2x = xLast1 + maxDist * cos(a1);
					double p2y = yLast1 + maxDist * sin(a1);
					double p3x = xLast1 + maxDist * cos(a1 + maxAng);
					double p3y = yLast1 + maxDist * sin(a1 + maxAng);
					
					Point coll = new Point();
					
					if (closestLineIntersection(xLast1, yLast1, p1x, p1y, coll)) {
						p1x = coll.x;
						p1y = coll.y;
					}
					
					if (closestLineIntersection(xLast1, yLast1, p2x, p2y, coll)) {
						p2x = coll.x;
						p2y = coll.y;
					}
					
					if (closestLineIntersection(xLast1, yLast1, p3x, p3y, coll)) {
						p3x = coll.x;
						p3y = coll.y;
					}
					
					xMax = maxFrom4(xLast1, p1x, p2x, p3x);
					xMin = minFrom4(xLast1, p1x, p2x, p3x);
					yMax = maxFrom4(yLast1, p1y, p2y, p3y);
					yMin = minFrom4(yLast1, p1y, p2y, p3y);
					
					if (pointDistance(xMax, yMax, xMin, yMin) < minDist) {
						break;
					}
				}
				
				if (xMax > mXMax) {
					xMax = mXMax;
				}
				if (xMin < mXMin) {
					xMin = mXMin;
				}
				if (yMax > mYMax) {
					yMax = mYMax;
				}
				if (yMin < mYMin) {
					yMin = mYMin;
				}

				while (attemptInner < maxAttemptsInner) {
					px = randInRange(xMin, xMax);
					py = randInRange(yMin, yMax);
					
					attemptInner++;
					genPoints++;
					
					ok = true;

					if (i == 0) {
						if (!isPointWithinRoutePolygon(px, py) || 
							!isPointOutsideInnerPolygons(px,py)) {
							ok = false;
						}
					} else {
						ROUTE_POINT p1 = r.get(i - 1);
						ROUTE_POINT p2 = new ROUTE_POINT();
						p2.px(px);
						p2.py(py);

						if (!isSegmentWithinRoutePolygon(p1, p2) || 
							!isSegmentOutsideInnerPolygons(p1,p2)) {
							ok = false;
						}
						
						if (pointDistance(p1, p2) < minDist) {
							ok = false;
						}
						
						if (i > 1) {
							double px1 = r.get(i - 2).px();
							double py1 = r.get(i - 2).py();
							double px2 = r.get(i - 1).px();
							double py2 = r.get(i - 1).py();
							double qx1 = r.get(i - 1).px();
							double qy1 = r.get(i - 1).py();
							double qx2 = px;
							double qy2 = py;
							
							if (abs(angleBetweenLines(px1, py1, px2, py2,
									qx1, qy1, qx2, qy2)) > maxAng) {
								ok = false;
							}
						}
					}
					
					if (ok) {
						break;
					}
				}

				if (!ok) {
					break;
				}

				ROUTE_POINT p = new ROUTE_POINT();
				p.px(px);
				p.py(py);
				p.speed(speed);
				p.time(0);
				r.add(p);
			}
			
			attemptOuter++;
			
			if (r.size() > rLargest.size()) {
				rLargest.clear();
				rLargest.addAll(r);
			}
			
			if (rLargest.size() == (length + start)) {
				break;
			}
		}
		
		mLastOuterAttempts = attemptOuter;
		mLastGeneratedPoints = genPoints;
		
		out.println("Generated points: " + genPoints +
				", Outer loops: " + attemptOuter);

		return rLargest;
	}

	public List<ROUTE_POINT> generateRouteWithin(int length,
			List<ROUTE_POINT> previous, double speed, int aheadMargin) {
		
		List<ROUTE_POINT> res = generateRouteWithin(length + aheadMargin, previous, speed);
		
		int start = 0;
		if (previous != null) {
			start = previous.size();
		}
		
		int end = length + start;
		if (end > res.size()) {
			end = res.size();
		}

		return res.subList(0, end);
	}
	
	public void setRandomSeed(long seed) {
		mRandom = new Random(seed);
	}
	
	public int getLastOuterAttempts() {
		return mLastOuterAttempts;
	}
	
	public int getLastGeneratedPoints() {
		return mLastGeneratedPoints;
	}

	public static double pointDistance(Point p1, Point p2) {
		return sqrt(pow(p2.x - p1.x, 2) + pow(p2.y - p1.y, 2));
	}
	
	public static double pointDistance(double p1x, double p1y, double p2x, double p2y) {
		return sqrt(pow(p2x - p1x, 2) + pow(p2y - p1y, 2));
	}
	
	public static double pointDistance(ROUTE_POINT p1, ROUTE_POINT p2) {
		return sqrt(pow(p2.px() - p1.px(), 2) + pow(p2.py() - p1.py(), 2));
	}
	
	public static double angleBetweenLines(
			double px1, double py1, double px2, double py2,
			double qx1, double qy1, double qx2, double qy2) {
		double a1 = atan2(py2 - py1, px2 - px1);
		double a2 = atan2(qy2 - qy1, qx2 - qx1);
		double diff = a2 - a1;
		
		if (diff > PI) {
			diff -= 2.0 * PI;
		} else if (diff < -PI) {
			diff += 2.0 * PI;
		}
		
		return diff;
	}
	
	public double randInRange(double min, double max) {
	    return mRandom.nextDouble() * (max - min) + min;
	}
	
	public boolean closestLineIntersection(double p0_x, double p0_y,
			double p1_x, double p1_y, Point coll) {
		if (mRoute == null || mRoute.size() < 2) {
			return false;
		}
		
		boolean res = false;
		Point collLast = new Point();
		double lastDist = 0.0;
		Point pStart = new Point(p0_x, p0_y);
		
		List<List<ROUTE_POINT>> routes = new ArrayList<List<ROUTE_POINT>>(mInnerHoles); 
		routes.add(mRoute);
		for ( List<ROUTE_POINT> route : routes) {
			for (int i = 0, j = route.size() - 1; i < route.size();j = i++) {
				if (getLineIntersection(p0_x, p0_y, p1_x, p1_y,
						route.get(i).px(), route.get(i).py(),
						route.get(j).px(), route.get(j).py(), collLast)) {
				
					if (res) {
						double dist = lastDist = pointDistance(pStart, collLast);
						if (dist < lastDist) {
							coll.setTo(collLast);
							lastDist = dist;
						}
					} else {
						coll.setTo(collLast);
						lastDist = pointDistance(pStart, collLast);
					}
					
					res = true;
				} //end if intersection
			}// end for each line of polygon			
		}// end for each polygon
		
		return res;
	}
	
	private static double minFrom4(double a, double b, double c, double d) {
		double res = a;
		if (b < res) {
			res = b;
		}
		if (c < res) {
			res = c;
		}
		if (d < res) {
			res = d;
		}
		return res;
	}
	
	private static double maxFrom4(double a, double b, double c, double d) {
		double res = a;
		if (b > res) {
			res = b;
		}
		if (c > res) {
			res = c;
		}
		if (d > res) {
			res = d;
		}
		return res;
	}

	private static boolean getLineIntersection(double p0_x, double p0_y, double p1_x, double p1_y, 
			double p2_x, double p2_y, double p3_x, double p3_y, Point coll)
	{
		double s1_x, s1_y, s2_x, s2_y;
		s1_x = p1_x - p0_x;     s1_y = p1_y - p0_y;
		s2_x = p3_x - p2_x;     s2_y = p3_y - p2_y;

		double s, t;
		s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y);
		t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y);

		if (s >= 0 && s <= 1 && t >= 0 && t <= 1) {
			// Collision detected
			coll.x = p0_x + (t * s1_x);
			coll.y = p0_y + (t * s1_y);
			return true;
		}

		return false; // No collision
	}

	private static boolean ccw(ROUTE_POINT A, ROUTE_POINT B, ROUTE_POINT C) {
		return (C.py() - A.py()) * (B.px() - A.px()) > 
		(B.py() - A.py()) * (C.px() - A.px());
	}

	private static boolean lineIntersect(ROUTE_POINT A, ROUTE_POINT B,
			ROUTE_POINT C, ROUTE_POINT D) {
		return ccw(A,C,D) != ccw(B,C,D) && ccw(A,B,C) != ccw(A,B,D);
	}
}
