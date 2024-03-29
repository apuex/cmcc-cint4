package com.github.apuex.cmcc.cint4.codec;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.List;

import org.slf4j.LoggerFactory;

import com.github.apuex.cmcc.cint4.AlarmModeAckCodec;
import com.github.apuex.cmcc.cint4.DynAccessModeAckCodec;
import com.github.apuex.cmcc.cint4.EnumPKType;
import com.github.apuex.cmcc.cint4.HeartBeatAckCodec;
import com.github.apuex.cmcc.cint4.HeartBeatCodec;
import com.github.apuex.cmcc.cint4.LoginAckCodec;
import com.github.apuex.cmcc.cint4.LoginCodec;
import com.github.apuex.cmcc.cint4.LogoutAckCodec;
import com.github.apuex.cmcc.cint4.LogoutCodec;
import com.github.apuex.cmcc.cint4.Message;
import com.github.apuex.cmcc.cint4.ModifyPAAckCodec;
import com.github.apuex.cmcc.cint4.ModifyPACodec;
import com.github.apuex.cmcc.cint4.SendAlarmAckCodec;
import com.github.apuex.cmcc.cint4.SendAlarmCodec;
import com.github.apuex.cmcc.cint4.SetAlarmModeCodec;
import com.github.apuex.cmcc.cint4.SetDynAccessModeCodec;
import com.github.apuex.cmcc.cint4.SetPointAckCodec;
import com.github.apuex.cmcc.cint4.SetPointCodec;
import com.github.apuex.cmcc.cint4.SyncAlarmAckCodec;
import com.github.apuex.cmcc.cint4.SyncAlarmCodec;
import com.github.apuex.cmcc.cint4.TATDArrayCodec;
import com.github.apuex.cmcc.cint4.TATDCodec;
import com.github.apuex.cmcc.cint4.TIDToSignalTypeMap;
import com.github.apuex.cmcc.cint4.TimeCheckAckCodec;
import com.github.apuex.cmcc.cint4.TimeCheckCodec;

import ch.qos.logback.classic.Logger;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.ByteToMessageDecoder;

public class ByteToCInt4MessageDecoder extends ByteToMessageDecoder {
	private static final Logger logger = (Logger) LoggerFactory.getLogger(ByteToCInt4MessageDecoder.class);
	
	@Override
	protected void decode(ChannelHandlerContext ctx, ByteBuf in, List<Object> out) throws Exception {
		final int READABLE = in.readableBytes();
		if (READABLE < 8) // sizeof(header) + sizeof(length)
			return; // frame header
		byte[] array = new byte[in.readableBytes()];
		in.getBytes(0, array, 0, array.length);
		ByteBuffer buf = ByteBuffer.wrap(array);
		buf.order(ByteOrder.LITTLE_ENDIAN);
		final int header = buf.getInt();
		if (Message.MESSAGE_HEADER != header) { // message header error
			System.out.printf("RCV%s : Message.MESSAGE_HEADER != 0x%04X\n", ctx.channel().remoteAddress(), header);
			ctx.close();
			return;
		}

		final int length = buf.getInt();
		if (READABLE < length)
			return; // frame header + length

		if (READABLE < 4)
			return; // serial no
		@SuppressWarnings("unused")
		final int serialNo = buf.getInt();

		if (READABLE < 4)
			return; // pk type
		final EnumPKType PKType = EnumPKType.fromValue(buf.getInt());

		buf.position(0);
		switch (PKType) {
		case LOGIN:
			out.add(LoginCodec.decode(buf));
			break;
		case LOGIN_ACK:
			out.add(LoginAckCodec.decode(buf));
			break;
		case LOGOUT:
			out.add(LogoutCodec.decode(buf));
			break;
		case LOGOUT_ACK:
			out.add(LogoutAckCodec.decode(buf));
			break;
		case SET_DYN_ACCESS_MODE:
			out.add(SetDynAccessModeCodec.decode(buf));
			break;
		case DYN_ACCESS_MODE_ACK:
			out.add(DynAccessModeAckCodec.decode(buf));
			break;
		case SET_ALARM_MODE:
			out.add(SetAlarmModeCodec.decode(buf));
			break;
		case ALARM_MODE_ACK:
			out.add(AlarmModeAckCodec.decode(buf));
			break;
		case SEND_ALARM:
			out.add(SendAlarmCodec.decode(buf));
			break;
		case SEND_ALARM_ACK:
			out.add(SendAlarmAckCodec.decode(buf));
			break;
		case SYNC_ALARM:
			out.add(SyncAlarmCodec.decode(buf));
			break;
		case SYNC_ALARM_ACK:
			out.add(SyncAlarmAckCodec.decode(buf));
			break;
		case SET_POINT:
			out.add(SetPointCodec.decode(buf));
			break;
		case SET_POINT_ACK:
			out.add(SetPointAckCodec.decode(buf));
			break;
		case MODIFY_PA:
			out.add(ModifyPACodec.decode(buf));
			break;
		case MODIFY_PA_ACK:
			out.add(ModifyPAAckCodec.decode(buf));
			break;
		case HEART_BEAT:
			out.add(HeartBeatCodec.decode(buf));
			break;
		case HEART_BEAT_ACK:
			out.add(HeartBeatAckCodec.decode(buf));
			break;
		case TIME_CHECK:
			out.add(TimeCheckCodec.decode(buf));
			break;
		case TIME_CHECK_ACK:
			out.add(TimeCheckAckCodec.decode(buf));
			break;
		default: // Unsupported PKType
			ctx.close();
			break;
		}
		StringBuilder sb = new StringBuilder();
		sb.append(String.format("[%s] DEC : bytes[%d] = { ", ctx.channel().remoteAddress(), buf.position()));
		for (int i = 0; i != buf.position(); ++i) {
			sb.append(String.format("%02X ", 0xff & array[i]));
		}
		sb.append("}");
		logger.info(sb.toString());
		in.skipBytes(buf.position());
	}

	public ByteToCInt4MessageDecoder(TIDToSignalTypeMap tidMap) {
		TATDCodec TATDCodec = new TATDCodec(tidMap);
		TATDArrayCodec TATDArrayCodec = new TATDArrayCodec(TATDCodec);
		AlarmModeAckCodec = new AlarmModeAckCodec();
		DynAccessModeAckCodec = new DynAccessModeAckCodec(TATDArrayCodec);
		HeartBeatAckCodec = new HeartBeatAckCodec();
		HeartBeatCodec = new HeartBeatCodec();
		LoginAckCodec = new LoginAckCodec();
		LoginCodec = new LoginCodec();
		LogoutAckCodec = new LogoutAckCodec();
		LogoutCodec = new LogoutCodec();
		ModifyPAAckCodec = new ModifyPAAckCodec();
		ModifyPACodec = new ModifyPACodec();
		SendAlarmAckCodec = new SendAlarmAckCodec();
		SendAlarmCodec = new SendAlarmCodec();
		SetAlarmModeCodec = new SetAlarmModeCodec();
		SetDynAccessModeCodec = new SetDynAccessModeCodec();
		SetPointAckCodec = new SetPointAckCodec();
		SetPointCodec = new SetPointCodec(TATDCodec);
		SyncAlarmAckCodec = new SyncAlarmAckCodec();
		SyncAlarmCodec = new SyncAlarmCodec();
		TimeCheckAckCodec = new TimeCheckAckCodec();
		TimeCheckCodec = new TimeCheckCodec();
	}

	private AlarmModeAckCodec AlarmModeAckCodec;
	private DynAccessModeAckCodec DynAccessModeAckCodec;
	private HeartBeatAckCodec HeartBeatAckCodec;
	private HeartBeatCodec HeartBeatCodec;
	private LoginAckCodec LoginAckCodec;
	private LoginCodec LoginCodec;
	private LogoutAckCodec LogoutAckCodec;
	private LogoutCodec LogoutCodec;
	private ModifyPAAckCodec ModifyPAAckCodec;
	private ModifyPACodec ModifyPACodec;
	private SendAlarmAckCodec SendAlarmAckCodec;
	private SendAlarmCodec SendAlarmCodec;
	private SetAlarmModeCodec SetAlarmModeCodec;
	private SetDynAccessModeCodec SetDynAccessModeCodec;
	private SetPointAckCodec SetPointAckCodec;
	private SetPointCodec SetPointCodec;
	private SyncAlarmAckCodec SyncAlarmAckCodec;
	private SyncAlarmCodec SyncAlarmCodec;
	private TimeCheckAckCodec TimeCheckAckCodec;
	private TimeCheckCodec TimeCheckCodec;
}
