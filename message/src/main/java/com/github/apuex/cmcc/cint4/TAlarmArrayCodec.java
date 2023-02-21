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

import java.nio.ByteBuffer;

/**
 * 告警的数组
 */
public class TAlarmArrayCodec {

	public void encode(ByteBuffer buf, TAlarmArray v) {
		buf.putInt(v.values.size());
		for (TAlarm e : v.values) {
			codec.encode(buf, e);
		}
	}

	public TAlarmArray decode(ByteBuffer buf) {
		TAlarmArray v = new TAlarmArray();
		final int size = buf.getInt();
		for (int i = 0; i != size; ++i) {
			v.values.add(codec.decode(buf));
		}
		return v;
	}

	private TAlarmCodec codec = new TAlarmCodec();
}
