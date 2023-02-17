package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登录
 */
public class Login extends Message { 
    public Login() {
        super(EnumPKType.LOGIN);
    }

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

    public static void encode(ByteBuffer buf, Login v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        Util.encodeString(buf, v.UserName, Lengths.USER_LENGTH);
        Util.encodeString(buf, v.PassWord, Lengths.PASSWORD_LENGTH);
        buf.putShort(v.CRC16);
    }

    public static Login decode(ByteBuffer buf) {
        Login v = new Login();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.UserName = Util.decodeString(buf, Lengths.USER_LENGTH);
        v.PassWord = Util.decodeString(buf, Lengths.PASSWORD_LENGTH);
        v.CRC16 = buf.getShort();
        return v;
    }

    public String UserName; // Login user name.
    public String PassWord; // Login password.
}

