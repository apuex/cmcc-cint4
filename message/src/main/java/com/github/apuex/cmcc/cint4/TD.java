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
 * 数字量的值的结构
 *
 * @author Wangxy
 */
public class TD extends TATD {
    private static final long serialVersionUID = 1L;

    public static void encode(ByteBuffer buf, TD v) {
        Util.encodeString(buf, v.SiteID, Lengths.SITEID_LENGTH);
        Util.encodeString(buf, v.DeviceID, Lengths.DEVICEID_LENGTH);
        Util.encodeString(buf, v.SignalID, Lengths.ID_LENGTH);
        Util.encodeString(buf, v.SignalNumber, Lengths.SIGNALNUM_LENGTH);
        buf.putInt(v.Value);
        buf.putInt(v.Status.getValue());
    }

    public static TD decode(ByteBuffer buf) {
        TD v = new TD();
        v.SiteID = Util.decodeString(buf, Lengths.SITEID_LENGTH);
        v.DeviceID = Util.decodeString(buf, Lengths.DEVICEID_LENGTH);
        v.SignalID = Util.decodeString(buf, Lengths.ID_LENGTH);
        v.SignalNumber = Util.decodeString(buf, Lengths.SIGNALNUM_LENGTH);
        v.Value = buf.getInt();
        v.Status = EnumState.fromValue(buf.getInt());
        return v;
    }

    @Override
    public boolean equals(Object o) {
        TD r = null;
        if(o instanceof TD) {
            r = (TD) o;
        } else {
            return false;
        }

        boolean result =
            ( this.SiteID.equals(r.SiteID)
            && this.DeviceID.equals(r.DeviceID)
            && this.SignalID.equals(r.SignalID)
            && this.SignalNumber.equals(r.SignalNumber)
            && this.Value == r.Value
            && this.Status == r.Status
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("TD { ")
            .append("SiteID=").append(this.SiteID)
            .append(", ").append("DeviceID=").append(this.DeviceID)
            .append(", ").append("SignalID=").append(this.SignalID)
            .append(", ").append("SignalNumber=").append(this.SignalNumber)
            .append(", ").append("Value=").append(this.Value)
            .append(", ").append("Status=").append(this.Status)
            .append(" }");

        return builder.toString();
    }

    public String SiteID; // 站点编号
    public String DeviceID; // 设备编号
    public String SignalID; // 监控点的6位信号编码，即《动环信号标准化字典表(20170927)》中的信号编码ID
    public String SignalNumber; // 同类监控点顺序号
    public int Value; // DI值
    public EnumState Status; // 数据值的状态
}

