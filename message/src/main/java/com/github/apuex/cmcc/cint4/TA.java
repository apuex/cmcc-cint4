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

/**
 * 模拟量的值的结构
 *
 * @author Wangxy
 */
public class TA extends TATD {
    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object o) {
        TA r = null;
        if(o instanceof TA) {
            r = (TA) o;
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
            .append("TA { ")
            .append("SiteID=").append(this.SiteID)
            .append(", ").append("DeviceID=").append(this.DeviceID)
            .append(", ").append("SignalID=").append(this.SignalID)
            .append(", ").append("SignalNumber=").append(this.SignalNumber)
            .append(", ").append("Value=").append(this.Value)
            .append(", ").append("Status=").append(this.Status)
            .append(" }");

        return builder.toString();
    }

    @Override
    public TID toTID() {
    	TID tid = new TID();
        tid.Type = EnumType.AI;
    	tid.SiteID = SiteID;
    	tid.DeviceID = DeviceID;
    	tid.SignalID = SignalID;
    	tid.SignalNumber = SignalNumber;
    	return tid;
    }

@Override
    public TID toDeviceTID() {
        TID tid = new TID();
        tid.Type = EnumType.DEVICE;
        tid.SiteID = SiteID;
        tid.DeviceID = DeviceID;
        tid.SignalID = SignalID;
        tid.SignalNumber = SignalNumber;
        return tid;
    }

    @Override
    public TID toSiteTID() {
        TID tid = new TID();
        tid.Type = EnumType.SITE;
        tid.SiteID = SiteID;
        tid.DeviceID = DeviceID;
        tid.SignalID = SignalID;
        tid.SignalNumber = SignalNumber;
        return tid;
    }

    public String SiteID; // 站点编号
    public String DeviceID; // 设备编号
    public String SignalID; // 监控点的6位信号编码，即《动环信号标准化字典表(20170927)》中的信号编码ID
    public String SignalNumber; // 同类监控点顺序号
    public float Value; // AI值
    public EnumState Status; // 数据值的状态
}

