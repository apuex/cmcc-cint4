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
 * 请求实时数据方式设置
 *
 * @author Wangxy
 */
public class SetDynAccessModeCodec {

    public void encode(ByteBuffer buf, SetDynAccessMode v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.TerminalID);
        buf.putInt(v.GroupID);
        buf.putInt(v.Mode.getValue());
        buf.putInt(v.PollingTime);
        this.IdsCodec.encode(buf, v.Ids);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
        buf.position(initialPos + 4);
        buf.putInt(pos - initialPos);
        buf.position(pos - 2);
        buf.putShort(Util.CRC16(buf.array(), initialPos, pos - 2));
    }

    public SetDynAccessMode decode(ByteBuffer buf) {
        SetDynAccessMode v = new SetDynAccessMode();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.TerminalID = buf.getInt();
        v.GroupID = buf.getInt();
        v.Mode = EnumAccessMode.fromValue(buf.getInt());
        v.PollingTime = buf.getInt();
        v.Ids = this.IdsCodec.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    public TIDArrayCodec IdsCodec = new TIDArrayCodec(); // 如果类型是站点，即获取站内所有设备下的监控点数据；如果是设备，即获取该设备下所有监控点数据；如果是监控点，即是该点数据。
}

