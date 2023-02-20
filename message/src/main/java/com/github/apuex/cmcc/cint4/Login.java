/*
 * Copyright (c) 2021-2023 WINCOM.
 * Copyright (c) 2021-2023 Wangxy <xtwxy@hotmail.com>
 *
 * All rights reserved. 
 *
 * This program and the accompanying materials
 * are made available under the terms of the Mozilla Public License 2.0.
 *
 * Contributors:
 *   Wangxy - initial implementation and documentation.
*/

package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 登录
 *
 * @author Wangxy
 */
public class Login extends Message {
    private static final long serialVersionUID = 1L;
    
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
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        Util.encodeString(buf, v.UserName, Lengths.USER_LENGTH);
        Util.encodeString(buf, v.PassWord, Lengths.PASSWORD_LENGTH);
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

    public static Login decode(ByteBuffer buf) {
        Login v = new Login();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.UserName = Util.decodeString(buf, Lengths.USER_LENGTH);
        v.PassWord = Util.decodeString(buf, Lengths.PASSWORD_LENGTH);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        Login r = null;
        if(o instanceof Login) {
            r = (Login) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.UserName.equals(r.UserName)
            && this.PassWord.equals(r.PassWord)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("Login { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("UserName=").append(this.UserName)
            .append(", ").append("PassWord=").append(this.PassWord)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public String UserName; // Login user name.
    public String PassWord; // Login password.
}

