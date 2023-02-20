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

/**
 * 告警的数组
 */
public class TAlarmArray implements Serializable {
    public static void encode(ByteBuffer buf, TAlarmArray v) {
    }

    public static TAlarmArray decode(ByteBuffer buf) {
        TAlarmArray v = new TAlarmArray();
	return v;
    }

}

