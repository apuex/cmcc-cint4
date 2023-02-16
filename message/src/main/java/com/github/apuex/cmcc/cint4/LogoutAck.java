package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登出响应
 */
public class LogoutAck extends Message {
    public LogoutAck
    ( 
    ) {
        super(EnumPKType.LOGOUT_ACK);
        
    }

    public LogoutAck
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.LOGOUT_ACK);
        
    }

    
}

