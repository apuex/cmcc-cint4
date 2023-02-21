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
 * 当前告警值的结构
 *
 * @author Wangxy
 */
public class TAlarmCodec {

    public void encode(ByteBuffer buf, TAlarm v) {
        Util.encodeString(buf, v.SCID, Lengths.SCID_LENGTH);
        Util.encodeString(buf, v.SerialNo, Lengths.ALARMSERIALNO_LENGTH);
        Util.encodeString(buf, v.SiteID, Lengths.SITEID_LENGTH);
        Util.encodeString(buf, v.DeviceID, Lengths.DEVICEID_LENGTH);
        Util.encodeString(buf, v.SignalID, Lengths.ID_LENGTH);
        Util.encodeString(buf, v.SignalNumber, Lengths.SIGNALNUM_LENGTH);
        Util.encodeString(buf, v.NMAlarmID, Lengths.ID_LENGTH);
        Util.encodeString(buf, v.AlarmTime, Lengths.TIME_LENGTH);
        buf.putInt(v.AlarmLevel.getValue());
        buf.putInt(v.AlarmFlag.getValue());
        buf.putFloat(v.EventValue);
        Util.encodeString(buf, v.AlarmDesc, Lengths.DES_LENGTH);
    }

    public TAlarm decode(ByteBuffer buf) {
        TAlarm v = new TAlarm();
        v.SCID = Util.decodeString(buf, Lengths.SCID_LENGTH);
        v.SerialNo = Util.decodeString(buf, Lengths.ALARMSERIALNO_LENGTH);
        v.SiteID = Util.decodeString(buf, Lengths.SITEID_LENGTH);
        v.DeviceID = Util.decodeString(buf, Lengths.DEVICEID_LENGTH);
        v.SignalID = Util.decodeString(buf, Lengths.ID_LENGTH);
        v.SignalNumber = Util.decodeString(buf, Lengths.SIGNALNUM_LENGTH);
        v.NMAlarmID = Util.decodeString(buf, Lengths.ID_LENGTH);
        v.AlarmTime = Util.decodeString(buf, Lengths.TIME_LENGTH);
        v.AlarmLevel = EnumState.fromValue(buf.getInt());
        v.AlarmFlag = EnumFlag.fromValue(buf.getInt());
        v.EventValue = buf.getFloat();
        v.AlarmDesc = Util.decodeString(buf, Lengths.DES_LENGTH);
        return v;
    }

    
}

