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

/**
 * 数据值的结构
 *
 * @author Wangxy
 */
public class TID implements Serializable {
    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object o) {
        TID r = null;
        if(o instanceof TID) {
            r = (TID) o;
        } else {
            return false;
        }

        boolean result =
            ( this.SiteID.equals(r.SiteID)
            && this.DeviceID.equals(r.DeviceID)
            && this.SignalID.equals(r.SignalID)
            && this.SignalNumber.equals(r.SignalNumber)
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("TID { ")
            .append("SiteID=").append(this.SiteID)
            .append(", ").append("DeviceID=").append(this.DeviceID)
            .append(", ").append("SignalID=").append(this.SignalID)
            .append(", ").append("SignalNumber=").append(this.SignalNumber)
            .append(" }");

        return builder.toString();
    }

    public String SiteID; // 站点编号
    public String DeviceID; // 设备编号
    public String SignalID; // 监控点的6位信号编码，即《动环信号标准化字典表(20170927)》中的信号编码ID
    public String SignalNumber; // 同类监控点顺序号
}

