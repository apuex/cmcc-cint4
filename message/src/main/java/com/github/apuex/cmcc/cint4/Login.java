package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登录
 */
public class Login extends Message {
    public Login
    ( String UserName
    , String PassWord
    ) {
        super(EnumPKType.LOGIN);
        this.UserName = UserName;
        this.PassWord = PassWord;
    }

    public Login
    ( int SerialNo
    , String UserName
    , String PassWord
    ) {
        super(SerialNo, EnumPKType.LOGIN);
        this.UserName = UserName;
        this.PassWord = PassWord;
    }

    public String UserName; // Login user name.
    public String PassWord; // Login password.
}

