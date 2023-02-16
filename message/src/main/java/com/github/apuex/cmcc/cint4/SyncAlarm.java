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

    
}

