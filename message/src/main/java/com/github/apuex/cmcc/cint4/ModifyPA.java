package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 改口令请求
 */
public class ModifyPA extends Message {
    private static final long serialVersionUID = 1L;
    
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
        final int initialPos = buf.position();
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
        final int pos = buf.position();
        // Message LENGTH - envelope fields
	buf.position(initialPos + 4);
	buf.putInt(pos - initialPos);
	buf.position(pos - 2);
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos - 2));
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

    @Override
    public boolean equals(Object o) {
        ModifyPA r = null;
        if(o instanceof ModifyPA) {
            r = (ModifyPA) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.UserName.equals(r.UserName)
            && this.OldPassWord.equals(r.OldPassWord)
            && this.NewPassWord.equals(r.NewPassWord)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("ModifyPA { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("UserName=").append(this.UserName)
            .append(", ").append("OldPassWord=").append(this.OldPassWord)
            .append(", ").append("NewPassWord=").append(this.NewPassWord)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public String UserName; // Login user name.
    public String OldPassWord; // Login password.
    public String NewPassWord; // Login password.
}

