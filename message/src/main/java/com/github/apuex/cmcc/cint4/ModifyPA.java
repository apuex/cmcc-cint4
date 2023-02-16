package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 改口令请求
 */
public class ModifyPA extends Message {
    public ModifyPA
    ( String UserName
    , String OldPassWord
    , String NewPassWord
    ) {
        super(EnumPKType.MODIFY_PA);
        this.UserName = UserName;
        this.OldPassWord = OldPassWord;
        this.NewPassWord = NewPassWord;
    }

    public ModifyPA
    ( int SerialNo
    , String UserName
    , String OldPassWord
    , String NewPassWord
    ) {
        super(SerialNo, EnumPKType.MODIFY_PA);
        this.UserName = UserName;
        this.OldPassWord = OldPassWord;
        this.NewPassWord = NewPassWord;
    }

    public String UserName; // Login user name.
    public String OldPassWord; // Login password.
    public String NewPassWord; // Login password.
}

