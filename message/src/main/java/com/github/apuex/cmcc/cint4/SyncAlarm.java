package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警同步
 */
public class SyncAlarm extends Message { 
    public SyncAlarm
    ( 
    ) {
        super(EnumPKType.SYNC_ALARM);
        
    }

    public SyncAlarm
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.SYNC_ALARM);
        
    }

    public static void encode(ByteBuffer buf, SyncAlarm v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        
        buf.putShort(v.CRC16);
    }

    public static SyncAlarm decode(ByteBuffer buf) {
        SyncAlarm v = new SyncAlarm();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

