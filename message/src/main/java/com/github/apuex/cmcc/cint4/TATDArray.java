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
 * 值的数组
 */
public class TATDArray implements Serializable {
	private static final long serialVersionUID = 1L;

	public static void encode(ByteBuffer buf, TATDArray v) {
		buf.putInt(v.values.size());
		for (TATD e : v.values) {
			TATD.encode(buf, e);
		}
	}

	public static TATDArray decode(ByteBuffer buf) {
		TATDArray v = new TATDArray();
		final int size = buf.getInt();
		for (int i = 0; i != size; ++i) {
			v.values.add(TATD.decode(buf));
		}
		return v;
	}

	public List<TATD> values = new LinkedList<TATD>();
}
