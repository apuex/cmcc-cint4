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
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
	buf.position(initialPos + 4);
	buf.putInt(pos - initialPos);
	buf.position(pos - 2);
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos));
    }

    public static SyncAlarm decode(ByteBuffer buf) {
        SyncAlarm v = new SyncAlarm();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

