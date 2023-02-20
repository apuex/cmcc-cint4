/*
 * Copyright (c) 2021-2023 WINCOM.
 * Copyright (c) 2021-2023 Wangxy <xtwxy@hotmail.com>
 *
 * All rights reserved. 
 *
 * This program and the accompanying materials
 * are made available under the terms of the Mozilla Public License 2.0.
 *
 * Contributors:
 *   Wangxy - initial implementation and documentation.
*/

package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

/**
 * 告警的数组
 */
public class TAlarmArray implements Serializable {
	private static final long serialVersionUID = 1L;

	public static void encode(ByteBuffer buf, TAlarmArray v) {
		buf.putInt(v.values.size());
		for (TAlarm e : v.values) {
			TAlarm.encode(buf, e);
		}
	}

	public static TAlarmArray decode(ByteBuffer buf) {
		TAlarmArray v = new TAlarmArray();
		final int size = buf.getInt();
		for (int i = 0; i != size; ++i) {
			v.values.add(TAlarm.decode(buf));
		}
		return v;
	}

	public List<TAlarm> values = new LinkedList<TAlarm>();
}
