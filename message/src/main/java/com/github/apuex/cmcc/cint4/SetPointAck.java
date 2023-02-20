package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 写数据响应
 */
public class SetPointAck extends Message {
    
    public SetPointAck() {
        super(EnumPKType.SET_POINT_ACK);
    }

    public SetPointAck
    ( TID Id
    , EnumResult Result
    ) {
        super(EnumPKType.SET_POINT_ACK);
        this.Id = Id;
        this.Result = Result;
    }

    public SetPointAck
    ( int SerialNo
    , TID Id
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SET_POINT_ACK);
        this.Id = Id;
        this.Result = Result;
    }

    public static void encode(ByteBuffer buf, SetPointAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        TID.encode(buf, v.Id);
        buf.putInt(v.Result.getValue());
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

    public static SetPointAck decode(ByteBuffer buf) {
        SetPointAck v = new SetPointAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Id = TID.decode(buf);
        v.Result = EnumResult.fromValue(buf.getInt());
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        SetPointAck r = null;
        if(o instanceof SetPointAck) {
            r = (SetPointAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Id.equals(r.Id)
            && this.Result == r.Result
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SetPointAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Id=").append(this.Id)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public TID Id; // 相应的值，数据的值的类型由相应的数据结构决定，数据结构中已经包含了监控点ID，因此上面的ID是冗余的
    public EnumResult Result; // 报文返回结果
}

