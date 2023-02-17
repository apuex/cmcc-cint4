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
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        TID.encode(buf, v.Id);
        buf.putInt(v.Result.getValue());
        buf.putShort(v.CRC16);
    }

    public static SetPointAck decode(ByteBuffer buf) {
        SetPointAck v = new SetPointAck();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.Id = TID.decode(buf);
        v.Result = EnumResult.fromValue(buf.getInt());
        v.CRC16 = buf.getShort();
        return v;
    }

    public TID Id; // 相应的值，数据的值的类型由相应的数据结构决定，数据结构中已经包含了监控点ID，因此上面的ID是冗余的
    public EnumResult Result; // 报文返回结果
}

