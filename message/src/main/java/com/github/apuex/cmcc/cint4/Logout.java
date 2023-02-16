package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登出
 */
public class Logout extends Message {
    public Logout
    ( 
    ) {
        super(EnumPKType.LOGOUT);
        
    }

    public Logout
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.LOGOUT);
        
    }

    
}

