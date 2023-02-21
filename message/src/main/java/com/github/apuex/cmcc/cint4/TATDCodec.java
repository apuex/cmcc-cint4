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
import java.util.Map;

/**
 * AI、DI值的结构的父类
 */
public class TATDCodec {
	public TATDCodec(Map<TID, EnumType> typeMap) {
		this.typeMap = typeMap;
	}

	public void encode(ByteBuffer buf, TATD v) {
		if (v instanceof TA) {
			taCodec.encode(buf, (TA) v);
		} else if (v instanceof TD) {
			tdCodec.encode(buf, (TD) v);
		} else {
			throw new IllegalArgumentException(String.format("%s is not supported.", v));
		}
	}

	public TATD decode(ByteBuffer buf) {
		TATD v = null;
		TID tid = null;
		final int pos = buf.position();
		// the head of TA/TD is accidentally equivalent to TID ^_^
		// re-interpret the head to be a TID causes ACCIDENTAL COUPLING to TID
		// which SHOULD BE AVOIDED!
		tid = tidCodec.decode(buf);
		buf.position(pos); // reset to begining.
		EnumType dataType = typeMap.get(tid);
		switch (dataType) {
		case AI:
			v = taCodec.decode(buf);
			break;
		case DI:
			v = tdCodec.decode(buf);
			break;
		default:
			throw new IllegalArgumentException(String.format("%s is not supported.", dataType));
		}
		return v;
	}

	private TIDCodec					 tidCodec	= new TIDCodec();
	private TACodec						 taCodec	= new TACodec();
	private TDCodec						 tdCodec	= new TDCodec();
	private Map<TID, EnumType> typeMap;
}
