package rcontrolstationcomm;

import java.util.Collections;
import java.util.Iterator;
import org.bridj.BridJ;
import org.bridj.CRuntime;
import org.bridj.FlagSet;
import org.bridj.IntValuedEnum;
import org.bridj.Pointer;
import org.bridj.ann.Library;
import org.bridj.ann.Runtime;

/**
 * Wrapper for library <b>RControlStationComm</b><br>
 * This file was autogenerated by <a href="http://jnaerator.googlecode.com/">JNAerator</a>,<br>
 * a tool written by <a href="http://ochafik.com/">Olivier Chafik</a> that <a href="http://code.google.com/p/jnaerator/wiki/CreditsAndLicense">uses a few opensource projects.</a>.<br>
 * For help, please visit <a href="http://nativelibs4java.googlecode.com/">NativeLibs4Java</a> or <a href="http://bridj.googlecode.com/">BridJ</a> .
 */
@Library("RControlStationComm") 
@Runtime(CRuntime.class) 
public class RControlStationCommLibrary {
	static {
		BridJ.register();
	}
	/** enum values */
	public enum RC_MODE implements IntValuedEnum<RC_MODE > {
		RC_MODE_CURRENT(0),
		RC_MODE_DUTY(1),
		RC_MODE_PID(2),
		RC_MODE_CURRENT_BRAKE(3);
		RC_MODE(long value) {
			this.value = value;
		}
		public final long value;
		public long value() {
			return this.value;
		}
		public Iterator<RC_MODE > iterator() {
			return Collections.singleton(this).iterator();
		}
		public static IntValuedEnum<RC_MODE > fromValue(int value) {
			return FlagSet.fromValue(value, values());
		}
	};
	/** enum values */
	public enum mc_fault_code implements IntValuedEnum<mc_fault_code > {
		FAULT_CODE_NONE(0),
		FAULT_CODE_OVER_VOLTAGE(1),
		FAULT_CODE_UNDER_VOLTAGE(2),
		FAULT_CODE_DRV8302(3),
		FAULT_CODE_ABS_OVER_CURRENT(4),
		FAULT_CODE_OVER_TEMP_FET(5),
		FAULT_CODE_OVER_TEMP_MOTOR(6);
		mc_fault_code(long value) {
			this.value = value;
		}
		public final long value;
		public long value() {
			return this.value;
		}
		public Iterator<mc_fault_code > iterator() {
			return Collections.singleton(this).iterator();
		}
		public static IntValuedEnum<mc_fault_code > fromValue(int value) {
			return FlagSet.fromValue(value, values());
		}
	};
	/**
	 * Original signature : <code>bool rcsc_connectTcp(const char*, int)</code><br>
	 * <i>native declaration : line 26</i>
	 */
	native public static boolean rcsc_connectTcp(Pointer<Byte > host, int port);
	/**
	 * Original signature : <code>void rcsc_disconnectTcp()</code><br>
	 * <i>native declaration : line 27</i>
	 */
	native public static void rcsc_disconnectTcp();
	/**
	 * Original signature : <code>void rcsc_setDebugLevel(int)</code><br>
	 * <i>native declaration : line 28</i>
	 */
	native public static void rcsc_setDebugLevel(int level);
	/**
	 * Original signature : <code>bool rcsc_hasError()</code><br>
	 * <i>native declaration : line 29</i>
	 */
	native public static boolean rcsc_hasError();
	/**
	 * Original signature : <code>char* rcsc_lastError()</code><br>
	 * <i>native declaration : line 30</i>
	 */
	native public static Pointer<Byte > rcsc_lastError();
	/**
	 * Original signature : <code>void rcsc_clearBuffers()</code><br>
	 * <i>native declaration : line 31</i>
	 */
	native public static void rcsc_clearBuffers();
	/**
	 * Original signature : <code>bool rcsc_getState(int, CAR_STATE*, int)</code><br>
	 * <i>native declaration : line 31</i>
	 */
	native public static boolean rcsc_getState(int car, Pointer<CAR_STATE > state, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_getEnuRef(int, bool, double*, int)</code><br>
	 * <i>native declaration : line 32</i>
	 */
	native public static boolean rcsc_getEnuRef(int car, boolean fromMap, Pointer<Double > llh, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_setEnuRef(int, double*, int)</code><br>
	 * <i>native declaration : line 33</i>
	 */
	native public static boolean rcsc_setEnuRef(int car, Pointer<Double > llh, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_addRoutePoints(int, ROUTE_POINT*, int, bool, bool, int, int)</code><br>
	 * <i>native declaration : line 34</i>
	 */
	native public static boolean rcsc_addRoutePoints(int car, Pointer<ROUTE_POINT > route, int len, boolean replace, boolean mapOnly, int mapRoute, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_clearRoute(int, int, int)</code><br>
	 * <i>native declaration : line 37</i>
	 */
	native public static boolean rcsc_clearRoute(int car, int mapRoute, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_setAutopilotActive(int, bool, int)</code><br>
	 * <i>native declaration : line 38</i>
	 */
	native public static boolean rcsc_setAutopilotActive(int car, boolean active, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_rcControl(int, int, double, double)</code><br>
	 * <i>native declaration : line 39</i>
	 */
	native public static boolean rcsc_rcControl(int car, int mode, double value, double steering);
	/**
	 * Original signature : <code>bool rcsc_getRoutePoints(int, ROUTE_POINT*, int*, int, int, int)</code><br>
	 * <i>native declaration : line 40</i>
	 */
	native public static boolean rcsc_getRoutePoints(int car, Pointer<ROUTE_POINT > route, Pointer<Integer > len, int maxLen, int mapRoute, int timeoutMs);
	/**
	 * Original signature : <code>bool rcsc_sendTerminalCmd(int, char*, char*, int)</code><br>
	 * <i>native declaration : /usr/include/rcontrolstationcomm_types.h:74</i>
	 */
	native public static boolean rcsc_sendTerminalCmd(int car, Pointer<Byte > cmd, Pointer<Byte > reply, int timeoutMs);
}