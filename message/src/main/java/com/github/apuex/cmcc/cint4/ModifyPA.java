package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 改口令请求
 */
public class ModifyPA extends Message { 
    public ModifyPA() {
        super(EnumPKType.MODIFY_PA);
    }

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

    public static void encode(ByteBuffer buf, ModifyPA v) {
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        Util.encodeString(buf, v.UserName, Lengths.USER_LENGTH);
        Util.encodeString(buf, v.OldPassWord, Lengths.PASSWORD_LENGTH);
        Util.encodeString(buf, v.NewPassWord, Lengths.PASSWORD_LENGTH);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
    }

    public static ModifyPA decode(ByteBuffer buf) {
        ModifyPA v = new ModifyPA();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.UserName = Util.decodeString(buf, Lengths.USER_LENGTH);
        v.OldPassWord = Util.decodeString(buf, Lengths.PASSWORD_LENGTH);
        v.NewPassWord = Util.decodeString(buf, Lengths.PASSWORD_LENGTH);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    public String UserName; // Login user name.
    public String OldPassWord; // Login password.
    public String NewPassWord; // Login password.
}

