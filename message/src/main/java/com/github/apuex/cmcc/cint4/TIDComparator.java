package com.github.apuex.cmcc.cint4;

import java.util.Comparator;

public class TIDComparator implements Comparator<TID> {

	@Override
	public int compare(TID l, TID r) {
		int result = 0;
		result = l.SiteID.compareTo(r.SiteID);
		if(result != 0) return result;
		result = l.DeviceID.compareTo(r.DeviceID);
		if(result != 0) return result;
		result = l.SignalID.compareTo(r.SignalID);
		if(result != 0) return result;
		result = l.SignalNumber.compareTo(r.SignalNumber);
		if(result != 0) return result;
		result = l.SiteID.compareTo(r.SiteID);

		return result;
	}

}
